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

  SourceManager &SM = *Result.SourceManager;
  const LangOptions &LangOpts = getLangOpts();
  const auto DeclGroup = DeclStatement->getDeclGroup();

  const std::string CurrentIndent =
      getCurrentLineIndent(DeclStatement->getLocStart(), SM);
  const std::string UserWrittenType = getUserWrittenType(DeclStatement, SM);

  std::string AllSingleDeclarations;
  SourceRange VariableLocation;

  // We will iterate through the declaration group and split it into
  // single declarations. For example:
  // int *p, q = 2, v;
  // - UserWrittenType will be int
  // - First iteration will cut-off 'int *p' and set locations accordingly
  // - Next iteration will start after the comma and so cut-off 'q = 2'
  //     - 'UserWrittenType q = 2' will be saved
  // - Next iteration will cut-off v
  //     - 'UserWrittenType v' will be saved
  // - and so on...
  for (auto It = DeclGroup.begin(); It != DeclGroup.end(); ++It) {

    std::string SingleDeclaration = UserWrittenType + " ";

    if (const auto *DecDecl = dyn_cast<const DeclaratorDecl>(*It)) {
      VariableLocation.setEnd(DecDecl->getLocEnd());
    } else if (const auto *TypeDecl = dyn_cast<const TypedefDecl>(*It)) {
      VariableLocation.setEnd(TypeDecl->getLocEnd());
    } else {
      llvm_unreachable(
          "Declaration is neither a DeclaratorDecl nor a TypedefDecl");
    }

    if (It == DeclGroup.begin()) {
      VariableLocation.setBegin(DeclStatement->getLocStart());
    }

    std::string VariableLocationStr =
        Lexer::getSourceText(CharSourceRange::getTokenRange(VariableLocation),
                             SM, LangOpts)
            .trim();

    // Check for pre-processor directive and add appropriate newline
    if (VariableLocationStr[0] == '#') {
      VariableLocationStr.insert(0, "\n");
    }

    if (It == DeclGroup.begin()) {
      SingleDeclaration = VariableLocationStr;
    } else {
      SingleDeclaration += VariableLocationStr;
    }

    // Workaround for
    // http://lists.llvm.org/pipermail/cfe-dev/2016-November/051425.html
    if (const auto *VariableDecl = dyn_cast<const VarDecl>(*It)) {

      if (VariableDecl->getType().getCanonicalType()->isScalarType() &&
          VariableDecl->hasInit() &&
          VariableDecl->getInitStyle() == VarDecl::CallInit) {

        const auto PP =
            Lexer::findLocationAfterToken(VariableLocation.getEnd(),
                                          tok::r_paren, SM, LangOpts, true)
                .getLocWithOffset(-1);

        const std::string Appendee = Lexer::getSourceText(
            CharSourceRange::getTokenRange(
                VariableLocation.getEnd().getLocWithOffset(1), PP),
            SM, LangOpts);

        SingleDeclaration += Appendee;
        VariableLocation.setEnd(
            VariableLocation.getEnd().getLocWithOffset(Appendee.size()));
      }
    }

    AllSingleDeclarations += SingleDeclaration + ";\n" + CurrentIndent;

    // Lookout for next location start
    const std::vector<tok::TokenKind> Tokens = {tok::semi, tok::comma};
    VariableLocation.setBegin(tidy::utils::lexer::findLocationAfterToken(
        VariableLocation.getEnd(), Tokens, *Result.Context));
  }

  if (!AllSingleDeclarations.empty()) {

    // Remove last indent and '\n'
    AllSingleDeclarations = StringRef(AllSingleDeclarations).rtrim();

    auto Diag = diag(DeclStatement->getSourceRange().getBegin(),
                     "declaration statement can be split up into single "
                     "line declarations");
    Diag << FixItHint::CreateReplacement(DeclStatement->getSourceRange(),
                                         AllSingleDeclarations);
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
