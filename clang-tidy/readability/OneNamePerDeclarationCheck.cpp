//===--- OneNamePerDeclarationCheck.cpp - clang-tidy-----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "OneNamePerDeclarationCheck.h"
#include "../utils/LexerUtils.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Lex/Lexer.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace readability {

const internal::VariadicDynCastAllOfMatcher<Decl, TagDecl> tagDecl;

static bool isWhitespaceExceptNL(unsigned char c);
static std::string getCurrentLineIndent(SourceLocation Loc,
                                        const SourceManager &SM);

void OneNamePerDeclarationCheck::registerMatchers(MatchFinder *Finder) {

  // Matches all non-single declaration within a compound statement {...}.
  // Unless, the variable declaration is a object definition directly after
  // a tag declaration (e.g. struct, class etc.):
  // class A { } Object1, Object2;  <-- won't be matched
  Finder->addMatcher(
      declStmt(allOf(hasParent(compoundStmt()),
                     unless(hasDescendant(tagDecl())), unless(declCountIs(1))))
          .bind("declstmt"),
      this);
}

void OneNamePerDeclarationCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *DeclStatement = Result.Nodes.getNodeAs<DeclStmt>("declstmt");
  if (DeclStatement == nullptr)
    return;

  // Macros will be ignored
  if (DeclStatement->getLocStart().isMacroID())
    return;

  auto Diag = diag(DeclStatement->getSourceRange().getBegin(),
                   "declaration statement can be split up into single "
                   "line declarations");

  SourceManager &SM = *Result.SourceManager;
  const LangOptions &LangOpts = getLangOpts();
  const auto DeclGroup = DeclStatement->getDeclGroup();

  const std::string CurrentIndent =
      getCurrentLineIndent(DeclStatement->getLocStart(), SM);
  const std::string UserWrittenType = getUserWrittenType(DeclStatement, SM);

  // We will iterate through the declaration group starting with the second
  // declaration. Then, the previous comma will be searched and replaced by a
  // ';' and the UserWrittenType inserted after it.
  for (auto It = DeclGroup.begin() + 1; It != DeclGroup.end(); ++It) {

    SourceLocation NameLocation;
    if (const auto *DecDecl = dyn_cast<const DeclaratorDecl>(*It)) {
      NameLocation = DecDecl->getLocation();
    } else if (const auto *TypeDecl = dyn_cast<const TypedefDecl>(*It)) {
      NameLocation = TypeDecl->getLocation();
    } else {
      llvm_unreachable(
          "Declaration is neither a DeclaratorDecl nor a TypedefDecl");
    }

    auto CommaLocation = utils::lexer::findTokenLocationBackward(
        *Result.Context, NameLocation, tok::comma);
    if (CommaLocation.isValid()) {
      SourceRange CommaRange(CommaLocation, CommaLocation);
      SourceRange AfterCommaToVarNameRange(CommaLocation.getLocWithOffset(1),
                                           NameLocation);
      const std::string AnyTokenBetweenCommaAndVarName =
          Lexer::getSourceText(
              CharSourceRange::getTokenRange(AfterCommaToVarNameRange), SM,
              LangOpts)
              .ltrim(); // may be &, *, etc.

      Diag << FixItHint::CreateReplacement(CommaRange, ";")
           << FixItHint::CreateReplacement(AfterCommaToVarNameRange,
                                           "\n" + CurrentIndent +
                                               UserWrittenType + " " +
                                               AnyTokenBetweenCommaAndVarName);
    }
  }
}

std::string
OneNamePerDeclarationCheck::getUserWrittenType(const DeclStmt *DeclStmt,
                                               SourceManager &SM) {
  const auto FirstVarIt = DeclStmt->getDeclGroup().begin();

  SourceLocation Location;
  size_t NameSize = 0;
  QualType Type;

  if (const auto *FirstVar = dyn_cast<const DeclaratorDecl>(*FirstVarIt)) {
    Location = FirstVar->getLocation();
    NameSize = FirstVar->getName().size();
    Type = FirstVar->getType();
  } else if (const auto *FirstVar = dyn_cast<const TypedefDecl>(*FirstVarIt)) {
    Location = FirstVar->getLocation();
    NameSize = FirstVar->getName().size();

    Type = FirstVar->getTypeSourceInfo()->getType();
    if (Type->isLValueReferenceType()) {
      Type = Type->getPointeeType();
    }
  } else {
    llvm_unreachable(
        "Declaration is neither a DeclaratorDecl nor a TypedefDecl");
  }

  SourceRange FVLoc(DeclStmt->getLocStart(), Location);

  std::string FVStr = Lexer::getSourceText(
      CharSourceRange::getTokenRange(FVLoc), SM, getLangOpts());

  FVStr.erase(FVStr.size() - NameSize); // remove var name
  std::string UserWrittenType = StringRef(FVStr).trim();

  // UserWrittenType might be and we want ->
  // const int S::* -> const int
  // const int *&   -> const int
  // long **        -> long int

  if (Type->isFunctionPointerType() || Type->isFunctionProtoType()) {
    auto Pos = UserWrittenType.find('(');
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.
      UserWrittenType.erase(Pos);
      UserWrittenType = StringRef(UserWrittenType).trim();
    }

    return UserWrittenType;
  }

  if (Type->isPointerType() || Type->isArrayType() || Type->isReferenceType()) {
    auto Pos = UserWrittenType.find_last_not_of("&*");
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.
      UserWrittenType.erase(Pos + 1);
      UserWrittenType = StringRef(UserWrittenType).trim();
    }

    return UserWrittenType;
  }

  if (const auto *MemberPointerT = Type->getAs<MemberPointerType>()) {
    auto Pos = UserWrittenType.find("::");
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.

      StringRef CN =
          MemberPointerT->getClass()->getCanonicalTypeInternal().getAsString();

      // CN will be 'struct/class Typename'. we are only interested in the
      // second part
      CN = CN.split(' ').second;
      Pos = UserWrittenType.rfind(CN, Pos);

      UserWrittenType.erase(Pos);
      UserWrittenType = StringRef(UserWrittenType).trim();
    }
  }

  return UserWrittenType;
}

static bool isWhitespaceExceptNL(unsigned char c) {
  switch (c) {
  case ' ':
  case '\t':
  case '\f':
  case '\v':
  case '\r':
    return true;
  default:
    return false;
  }
}

static std::string getCurrentLineIndent(SourceLocation Loc,
                                        const SourceManager &SM) {
  const auto V = SM.getDecomposedLoc(Loc);
  const FileID FID = V.first;
  const unsigned StartOffs = V.second;

  const StringRef MB = SM.getBufferData(FID);

  const unsigned LineNo = SM.getLineNumber(FID, StartOffs) - 1;
  const SrcMgr::ContentCache *Content =
      SM.getSLocEntry(FID).getFile().getContentCache();
  const unsigned LineOffs = Content->SourceLineCache[LineNo];

  // Find the whitespace at the start of the line.
  StringRef IndentSpace;
  {
    size_t i = LineOffs;
    while (isWhitespaceExceptNL(MB[i])) {
      ++i;
    }
    IndentSpace = MB.substr(LineOffs, i - LineOffs);
  }

  return IndentSpace;
}

} // namespace readability
} // namespace tidy
} // namespace clang
