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

static bool isWhitespaceExceptNL(unsigned char C);
static std::string removeMultiLineComments(std::string Str);
static std::string getCurrentLineIndent(SourceLocation Loc,
                                        const SourceManager &SM);

void OneNamePerDeclarationCheck::registerMatchers(MatchFinder *Finder) {

  // Matches all non-single declaration within a compound statement {...}.
  // Unless, the variable declaration is a object definition directly after
  // a tag declaration (e.g. struct, class etc.):
  // class A { } Object1, Object2;  <-- won't be matched
  Finder->addMatcher(
      declStmt(allOf(hasParent(compoundStmt()), hasDescendant(namedDecl()),
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

  SourceManager &SM = *Result.SourceManager;
  const LangOptions &LangOpts = getLangOpts();
  const auto DeclGroup = DeclStatement->getDeclGroup();

  const auto DeclStmtStart = DeclStatement->getLocStart();
  const std::string CurrentIndent = getCurrentLineIndent(DeclStmtStart, SM);
  const std::string UserWrittenType = getUserWrittenType(DeclStatement, SM);

  auto Diag = diag(
      DeclStmtStart,
      "declaration statement can be split up into single line declarations");

  // We will iterate through the declaration group starting with the second
  // declaration. Then, the previous comma will be searched and replaced by a
  // ';' and the UserWrittenType inserted after it.
  for (auto It = DeclGroup.begin() + 1; It != DeclGroup.end(); ++It) {

    const auto NameLocation = dyn_cast<const NamedDecl>(*It)->getLocation();
    const auto CommaLocation = utils::lexer::findTokenLocationBackward(
        *Result.Context, NameLocation, tok::comma);

    if (CommaLocation.isValid()) {
      const SourceRange AfterCommaToVarNameRange(
          CommaLocation.getLocWithOffset(1), NameLocation);
      std::string AnyTokenBetweenCommaAndVarName =
          Lexer::getSourceText(
              CharSourceRange::getTokenRange(AfterCommaToVarNameRange), SM,
              LangOpts)
              .ltrim(); // may be &, *, etc.

      // Check for pre-processor directive and add appropriate newline
      if (AnyTokenBetweenCommaAndVarName.front() == '#')
        AnyTokenBetweenCommaAndVarName.insert(0, "\n");

      Diag << FixItHint::CreateReplacement(CommaLocation, ";")
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

  const SourceRange FVLoc(DeclStmt->getLocStart(), Location);

  std::string FVStr = Lexer::getSourceText(
      CharSourceRange::getTokenRange(FVLoc), SM, getLangOpts());

  FVStr.erase(FVStr.size() - NameSize); // remove var name
  std::string UserWrittenType = StringRef(FVStr).trim();

  UserWrittenType = removeMultiLineComments(UserWrittenType);

  // UserWrittenType might be and we want ->
  // const int S::* -> const int
  // const int *&   -> const int
  // long **        -> long int

  if (Type->isFunctionPointerType() || Type->isFunctionProtoType()) {
    const auto Pos = UserWrittenType.find('(');
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.
      UserWrittenType.erase(Pos);
      UserWrittenType = StringRef(UserWrittenType).trim();
    }

    return UserWrittenType;
  }

  if (Type->isPointerType() || Type->isArrayType() || Type->isReferenceType()) {
    const auto Pos = UserWrittenType.find_first_of("&*");
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.
      UserWrittenType.erase(Pos);
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

static bool isWhitespaceExceptNL(unsigned char C) {
  switch (C) {
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

static std::string removeMultiLineComments(std::string Str) {
  auto Pos1 = Str.find("/*");
  while (Pos1 != std::string::npos) {
    const auto Pos2 = Str.find("*/", Pos1 + 1);
    Str.erase(Pos1, Pos2 - Pos1 + 2);
    Pos1 = Str.find("/*");
  }

  Str = StringRef(Str).trim();
  return Str;
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
