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
static std::string getCurrentIndent(SourceLocation Loc,
                                    const SourceManager &SM);

void OneNamePerDeclarationCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(declStmt(allOf(hasParent(compoundStmt()),
                                    unless(hasDescendant(tagDecl()))))
                         .bind("declstmt"),
                     this);
}

void OneNamePerDeclarationCheck::check(const MatchFinder::MatchResult &Result) {
  if (const auto *DeclStmt =
          Result.Nodes.getNodeAs<clang::DeclStmt>("declstmt")) {

    if (DeclStmt->isSingleDecl() == false &&
        DeclStmt->getLocStart().isMacroID() == false) {

      SourceManager &SM = *Result.SourceManager;
      const LangOptions &LangOpts = getLangOpts();

      const std::string CurrentIndent =
          getCurrentIndent(DeclStmt->getLocStart(), SM);
      std::string UserWrittenType = getUserWrittenType(DeclStmt, SM);

      std::string AllSingleDeclarations = "";
      SourceRange VariableLocation;

      for (auto it = DeclStmt->getDeclGroup().begin();
           it != DeclStmt->getDeclGroup().end(); ++it) {

        std::string SingleDeclaration = UserWrittenType + " ";

        if (const clang::DeclaratorDecl *DecDecl =
                llvm::dyn_cast<const clang::DeclaratorDecl>(*it)) {

          if (it == DeclStmt->getDeclGroup().begin())
            VariableLocation.setBegin(DeclStmt->getLocStart());

          VariableLocation.setEnd(DecDecl->getLocEnd());

          std::string VariableLocationStr =
              Lexer::getSourceText(
                  CharSourceRange::getTokenRange(VariableLocation), SM,
                  LangOpts)
                  .trim();

          // check for pre processor directive
          if (VariableLocationStr[0] == '#')
            VariableLocationStr.insert(0, "\n");

          if (it == DeclStmt->getDeclGroup().begin())
            SingleDeclaration = VariableLocationStr;
          else
            SingleDeclaration += VariableLocationStr;

          // workaround for
          // http://lists.llvm.org/pipermail/cfe-dev/2016-November/051425.html
          if (const clang::VarDecl *VarDecl =
                  llvm::dyn_cast<const clang::VarDecl>(*it)) {

            if (VarDecl->getType().getCanonicalType()->isScalarType() &&
                VarDecl->hasInit() &&
                VarDecl->getInitStyle() == clang::VarDecl::CallInit) {

              auto PP = Lexer::findLocationAfterToken(VariableLocation.getEnd(),
                                                      tok::r_paren, SM,
                                                      LangOpts, true)
                            .getLocWithOffset(-1);

              std::string Appendee = Lexer::getSourceText(
                  CharSourceRange::getTokenRange(
                      VariableLocation.getEnd().getLocWithOffset(1), PP),
                  SM, LangOpts);

              SingleDeclaration += Appendee;
              VariableLocation.setEnd(
                  VariableLocation.getEnd().getLocWithOffset(Appendee.size()));
            }
          }

          AllSingleDeclarations += SingleDeclaration + ";\n" + CurrentIndent;

          const std::vector<tok::TokenKind> tokens = {tok::semi, tok::comma};
          VariableLocation.setBegin(tidy::utils::lexer::findLocationAfterToken(
              VariableLocation.getEnd(), tokens, *Result.Context));
        }
      }

      if (AllSingleDeclarations.empty() == false) {

        // remove last indent and '\n'
        AllSingleDeclarations = StringRef(AllSingleDeclarations).rtrim();

        auto Diag = diag(DeclStmt->getSourceRange().getBegin(),
                         "declaration statement can be split up into single "
                         "line declarations");
        Diag << FixItHint::CreateReplacement(DeclStmt->getSourceRange(),
                                             AllSingleDeclarations);
      }
    }
  }
}

std::string
OneNamePerDeclarationCheck::getUserWrittenType(const clang::DeclStmt *DeclStmt,
                                               SourceManager &SM) {
  auto FirstVarIt = DeclStmt->getDeclGroup().begin();
  auto FirstVar = llvm::dyn_cast<const clang::DeclaratorDecl>(*FirstVarIt);

  assert(FirstVar != nullptr && "DeclStmt has no element!");

  SourceRange FVLoc(DeclStmt->getLocStart(), FirstVar->getLocation());

  std::string FVStr = Lexer::getSourceText(
      CharSourceRange::getTokenRange(FVLoc), SM, getLangOpts());

  FVStr.erase(FVStr.size() - FirstVar->getName().size()); // remove var name
  FVStr = StringRef(FVStr).trim();

  auto Type = FirstVar->getType();

  if (Type->isFunctionPointerType()) {
    auto Pos = FVStr.find('(');
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.
      FVStr.erase(Pos);
      FVStr = StringRef(FVStr).trim();
    }

    return FVStr;
  }

  if (Type->isPointerType() || Type->isArrayType() || Type->isReferenceType()) {
    auto Pos = FVStr.find_last_not_of("&*");
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.
      FVStr.erase(Pos + 1);
      FVStr = StringRef(FVStr).trim();
    }

    return FVStr;
  }

  if (const MemberPointerType *T = Type->getAs<MemberPointerType>()) {
    auto Pos = FVStr.find("::");
    if (Pos != std::string::npos) { // might be hidden behind typedef etc.

      StringRef CN = T->getClass()->getCanonicalTypeInternal().getAsString();

      // CN will be 'struct/class Typename'. we are only interested in the
      // second part
      CN = CN.split(' ').second;
      Pos = FVStr.rfind(CN, Pos);

      FVStr.erase(Pos);
      FVStr = StringRef(FVStr).trim();
    }
  }

  return FVStr;
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

static std::string getCurrentIndent(SourceLocation Loc,
                                    const SourceManager &SM) {
  auto V = SM.getDecomposedLoc(Loc);
  FileID FID = V.first;
  unsigned StartOffs = V.second;

  StringRef MB = SM.getBufferData(FID);

  unsigned LineNo = SM.getLineNumber(FID, StartOffs) - 1;
  const SrcMgr::ContentCache *Content =
      SM.getSLocEntry(FID).getFile().getContentCache();
  unsigned LineOffs = Content->SourceLineCache[LineNo];

  // find the whitespace at the start of the line.
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
