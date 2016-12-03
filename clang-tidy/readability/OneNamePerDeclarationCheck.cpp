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

static bool isWhitespaceExceptNL(unsigned char c);
static std::string keepIndentationAfterNewLine(std::string Str,
                                               SourceLocation Loc,
                                               const SourceManager &SM);
static std::string replaceAll(std::string Str, const std::string &From,
                              const std::string &To);

void OneNamePerDeclarationCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(declStmt(hasParent(compoundStmt())).bind("declstmt"),
                     this);
}

void OneNamePerDeclarationCheck::check(const MatchFinder::MatchResult &Result) {

  if (const auto *DeclStmt =
          Result.Nodes.getNodeAs<clang::DeclStmt>("declstmt")) {

    if (DeclStmt->isSingleDecl() == false) {

      SourceManager &SM = *Result.SourceManager;
      const LangOptions &LangOpts = getLangOpts();

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

          auto VariableLocationStr =
              Lexer::getSourceText(
                  CharSourceRange::getTokenRange(VariableLocation), SM,
                  LangOpts)
                  .trim();
          if (it == DeclStmt->getDeclGroup().begin())
            SingleDeclaration = VariableLocationStr;
          else
            SingleDeclaration += VariableLocationStr;

          SingleDeclaration = replaceAll(SingleDeclaration, "\n", "{|;");

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

          AllSingleDeclarations += SingleDeclaration + ";\n";

          const std::vector<tok::TokenKind> tokens = {tok::semi, tok::comma};
          VariableLocation.setBegin(tidy::utils::lexer::findLocationAfterToken(
              VariableLocation.getEnd(), tokens, *Result.Context));
        }
      }

      if (AllSingleDeclarations.empty() == false) {
        // remove last '\n'
        AllSingleDeclarations.pop_back();

        AllSingleDeclarations = keepIndentationAfterNewLine(
            AllSingleDeclarations, DeclStmt->getLocStart(), SM);

        AllSingleDeclarations = replaceAll(AllSingleDeclarations, "{|;", "\n");
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

  if(auto FFV = llvm::dyn_cast<const clang::VarDecl>(*FirstVarIt))
  {
    auto l = Lexer::getLocForEndOfToken(FFV->getTypeSourceInfo()->getTypeLoc().getLocEnd(),
                                        0, SM, getLangOpts());

    llvm::outs() << "0:" << Lexer::getSourceText(CharSourceRange::getTokenRange(
            DeclStmt->getLocStart(), FFV->getTypeSourceInfo()->getTypeLoc().getLocEnd()),
                                                 SM, getLangOpts())
                 << "\n";

    llvm::outs() << "1:" << Lexer::getSourceText(CharSourceRange::getTokenRange(
            DeclStmt->getLocStart(), l),
                                                 SM, getLangOpts())
                 << "\n";

    llvm::outs() << "2:" << Lexer::getSourceText(CharSourceRange::getCharRange(
            DeclStmt->getLocStart(), l),
                                                 SM, getLangOpts())
                 << "\n";
  }
  std::string FVStr = Lexer::getSourceText(
      CharSourceRange::getTokenRange(FVLoc), SM, getLangOpts());

  FVStr.erase(FVStr.size() - FirstVar->getName().size());
  FVStr = StringRef(FVStr).trim();

  auto Type = FirstVar->getType();

  if (Type->isFunctionPointerType()) {
    auto POS = FVStr.find('(');
    if (POS != std::string::npos) { // might be hidden behind typedef etc.
      FVStr.erase(POS);
      FVStr = StringRef(FVStr).trim();
    }
  } else if (Type->isPointerType() || Type->isArrayType()) {
    auto POS = FVStr.find_last_not_of('*');
    if (POS != std::string::npos) { // might be hidden behind typedef etc.
      FVStr.erase(POS + 1);
      FVStr = StringRef(FVStr).trim();
    }
  } else if (Type->isMemberPointerType() ||
             Type->isMemberFunctionPointerType()) {
    const MemberPointerType *T = Type->getAs<MemberPointerType>();

    std::string CN = T->getClass()->getCanonicalTypeInternal().getAsString();
    auto POS = CN.find("struct ");

    if (POS != std::string::npos)
      CN.erase(POS, std::string("struct ").size());

    POS = CN.find("class ");

    if (POS != std::string::npos)
      CN.erase(POS, std::string("class ").size());

    POS = FVStr.find("::");

    if (POS != std::string::npos) { // might be hidden behind typedef etc.
      POS = FVStr.rfind(CN, POS);

      FVStr.erase(POS);
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

static std::string keepIndentationAfterNewLine(std::string Str,
                                               SourceLocation Loc,
                                               const SourceManager &SM) {
  std::pair<FileID, unsigned> V = SM.getDecomposedLoc(Loc);
  FileID FID = V.first;
  unsigned StartOffs = V.second;

  StringRef MB = SM.getBufferData(FID);

  unsigned lineNo = SM.getLineNumber(FID, StartOffs) - 1;
  const SrcMgr::ContentCache *Content =
      SM.getSLocEntry(FID).getFile().getContentCache();
  unsigned lineOffs = Content->SourceLineCache[lineNo];

  // Find the whitespace at the start of the line.
  StringRef IndentSpace;
  {
    unsigned i = lineOffs;
    while (isWhitespaceExceptNL(MB[i]))
      ++i;
    IndentSpace = MB.substr(lineOffs, i - lineOffs);
  }

  StringRef Splitter = Str;
  SmallVector<StringRef, 4> Lines;
  SmallString<128> indentedStr;
  Splitter.split(Lines, "\n");

  for (unsigned i = 0, e = Lines.size(); i != e; ++i) {
    indentedStr += Lines[i];
    if (i < e - 1) {
      indentedStr += '\n';
      indentedStr += IndentSpace;
    }
  }
  return indentedStr.str();
}

static std::string replaceAll(std::string Str, const std::string &From,
                              const std::string &To) {
  size_t Pos = 0;
  while ((Pos = Str.find(From, Pos)) != std::string::npos) {
    Str.replace(Pos, From.length(), To);
    Pos += To.length();
  }
  return Str;
}

} // namespace readability
} // namespace tidy
} // namespace clang
