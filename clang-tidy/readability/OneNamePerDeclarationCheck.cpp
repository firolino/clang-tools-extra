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

using namespace clang;
using namespace clang::ast_matchers;
using namespace llvm;

namespace clang {
namespace tidy {
namespace readability {

const internal::VariadicDynCastAllOfMatcher<Decl, TagDecl> tagDecl;

static bool isWhitespaceExceptNL(unsigned char c);
static std::string getCurrentLineIndent(SourceLocation Loc,
                                        const SourceManager &SM);

void OneNamePerDeclarationCheck::registerMatchers(MatchFinder *Finder) {

  // Matches all declaration within a compound statement {...}.
  // Unless, the variable declaration is a object definition directly after
  // a tag declaration (e.g. struct, class etc.):
  // class A { } Object1, Object2;  <-- won't be matched
  Finder->addMatcher(declStmt(allOf(hasParent(compoundStmt()),
                                    unless(hasDescendant(tagDecl()))))
                         .bind("declstmt"),
                     this);
}

void OneNamePerDeclarationCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *DeclStmt = Result.Nodes.getNodeAs<clang::DeclStmt>("declstmt");
  if (DeclStmt == nullptr)
    return;

  // Single declarations and macros will be ignored
  if (DeclStmt->isSingleDecl() || DeclStmt->getLocStart().isMacroID())
    return;

  SourceManager &SM = *Result.SourceManager;
  const LangOptions &LangOpts = getLangOpts();

  const std::string CurrentIndent =
      getCurrentLineIndent(DeclStmt->getLocStart(), SM);
  const std::string UserWrittenType = getUserWrittenType(DeclStmt, SM);

  std::string AllSingleDeclarations = "";
  SourceRange VariableLocation;

  // We will iterate through the declaration group and split it into
  // single declarations. For example:
  // int *p, q = 2, v;
  // - UserWrittenType will be int
  // - Fist iteration will cut-off 'int *p' and set locations accordingly
  // - Next iteration will start after the comma and so cut-off 'q = 2'
  //     - 'UserWrittenType q = 2' will be saved
  // - Next iteration will cut-off v
  //     - 'UserWrittenType v' will be saved
  // - and so on...
  for (auto it = DeclStmt->getDeclGroup().begin();
       it != DeclStmt->getDeclGroup().end(); ++it) {

    std::string SingleDeclaration = UserWrittenType + " ";

    if (const clang::DeclaratorDecl *DecDecl =
            llvm::dyn_cast<const clang::DeclaratorDecl>(*it))
      VariableLocation.setEnd(DecDecl->getLocEnd());
    else if (const clang::TypedefDecl *TypeDecl =
                 llvm::dyn_cast<const clang::TypedefDecl>(*it))
      VariableLocation.setEnd(TypeDecl->getLocEnd());
    else
      llvm_unreachable(
          "Declaration is neither a DeclaratorDecl nor a TypedefDecl");

    if (it == DeclStmt->getDeclGroup().begin())
      VariableLocation.setBegin(DeclStmt->getLocStart());

    std::string VariableLocationStr =
        Lexer::getSourceText(CharSourceRange::getTokenRange(VariableLocation),
                             SM, LangOpts)
            .trim();

    // Check for pre-processor directive and add appropriate newline
    if (VariableLocationStr[0] == '#')
      VariableLocationStr.insert(0, "\n");

    if (it == DeclStmt->getDeclGroup().begin())
      SingleDeclaration = VariableLocationStr;
    else
      SingleDeclaration += VariableLocationStr;

    // Workaround for
    // http://lists.llvm.org/pipermail/cfe-dev/2016-November/051425.html
    if (const clang::VarDecl *VarDecl =
            llvm::dyn_cast<const clang::VarDecl>(*it)) {

      if (VarDecl->getType().getCanonicalType()->isScalarType() &&
          VarDecl->hasInit() &&
          VarDecl->getInitStyle() == clang::VarDecl::CallInit) {

        auto PP =
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
    const std::vector<tok::TokenKind> tokens = {tok::semi, tok::comma};
    VariableLocation.setBegin(tidy::utils::lexer::findLocationAfterToken(
        VariableLocation.getEnd(), tokens, *Result.Context));
  }

  if (AllSingleDeclarations.empty() == false) {

    // Remove last indent and '\n'
    AllSingleDeclarations = StringRef(AllSingleDeclarations).rtrim();

    auto Diag = diag(DeclStmt->getSourceRange().getBegin(),
                     "declaration statement can be split up into single "
                     "line declarations");
    Diag << FixItHint::CreateReplacement(DeclStmt->getSourceRange(),
                                         AllSingleDeclarations);
  }
}

std::string
OneNamePerDeclarationCheck::getUserWrittenType(const clang::DeclStmt *DeclStmt,
                                               SourceManager &SM) {
  auto FirstVarIt = DeclStmt->getDeclGroup().begin();

  SourceLocation Location;
  size_t NameSize = 0;
  QualType Type;

  if (const auto FirstVar =
          llvm::dyn_cast<const clang::DeclaratorDecl>(*FirstVarIt)) {
    Location = FirstVar->getLocation();
    NameSize = FirstVar->getName().size();
    Type = FirstVar->getType();
  } else if (const auto FirstVar =
                 llvm::dyn_cast<const clang::TypedefDecl>(*FirstVarIt)) {
    Location = FirstVar->getLocation();
    NameSize = FirstVar->getName().size();

    Type = FirstVar->getTypeSourceInfo()->getType();
    if (Type->isLValueReferenceType())
      Type = Type->getPointeeType();
  } else
    llvm_unreachable(
        "Declaration is neither a DeclaratorDecl nor a TypedefDecl");

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

  if (const MemberPointerType *T = Type->getAs<MemberPointerType>()) {
    auto Pos = UserWrittenType.find("::");
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.

      StringRef CN = T->getClass()->getCanonicalTypeInternal().getAsString();

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
    while (isWhitespaceExceptNL(MB[i]))
      ++i;
    IndentSpace = MB.substr(LineOffs, i - LineOffs);
  }

  return IndentSpace;
}

} // namespace readability
} // namespace tidy
} // namespace clang
