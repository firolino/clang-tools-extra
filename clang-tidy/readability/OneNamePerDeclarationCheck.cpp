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

static std::string removeMultiLineComments(std::string Str);
static std::string getCurrentLineIndent(SourceLocation Loc,
                                        const SourceManager &SM);

void OneNamePerDeclarationCheck::registerMatchers(MatchFinder *Finder) {
  // Matches all non-single declaration within a compound statement {...}.
  Finder->addMatcher(declStmt(hasParent(compoundStmt()),
                              hasDescendant(namedDecl()),
                              unless(declCountIs(1)))
                         .bind("declstmt"),
                     this);
}

void OneNamePerDeclarationCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *DeclStatement = Result.Nodes.getNodeAs<DeclStmt>("declstmt");
  if (!DeclStatement){ TEST check this, when running on llvm source
    return;

  // Macros will be ignored
  if (DeclStatement->getLocStart().isMacroID())
    return;

  bool IsTagDef = false;
  const DeclGroupRef DeclGroup = DeclStatement->getDeclGroup();

  if (const auto *TagDef = dyn_cast<TagDecl>(*DeclGroup.begin())) {
    // Ignore:
    //  - Anonymous struct, class, enum or union
    //  - Single variable definitons (e.g. struct S {} a;)
    //     - First element is the TagDecl and second the variable, thus 2 below
    if (TagDef->getName().empty() || DeclGroup.getDeclGroup().size() == 2)
      return;

    IsTagDef = true;
  }

  SourceManager &SM = *Result.SourceManager;
  const LangOptions &LangOpts = getLangOpts();

  const SourceLocation DeclStmtStart = DeclStatement->getLocStart();
  const std::string CurrentIndent = getCurrentLineIndent(DeclStmtStart, SM);
  const std::string UserWrittenType = getUserWrittenType(DeclStatement, SM);

  auto Diag =
      diag(DeclStmtStart, "multiple declarations should be split into "
                          "individual declarations to improve readability");

  // We will iterate through the declaration group starting with the second
  // declaration. Then, the previous comma will be searched and replaced by a
  // ';' and the UserWrittenType inserted after it.
  // For a statement that is a tag + var definition (e.g. struct S {} a,b;),
  // we will start looking for a closing }-brace instead a comma.

  auto SecondElement = DeclGroup.begin() + 1;

  for (auto It = SecondElement; It != DeclGroup.end(); ++It) {
    const auto NameLocation = dyn_cast<NamedDecl>(*It)->getLocation();
    SourceLocation CommaLocation = utils::lexer::findTokenLocationBackward(
        *Result.Context, NameLocation, tok::comma);

    // If we are starting with a TagDecl, find the closing }-brace
    // instead a comma
    if (IsTagDef && (It == SecondElement))
      CommaLocation = utils::lexer::findTokenLocationBackward(
          *Result.Context, NameLocation, tok::r_brace);

    if (CommaLocation.isValid()) {
      const SourceRange AfterCommaToVarNameRange(
          CommaLocation.getLocWithOffset(1), NameLocation);
      std::string AnyTokenBetweenCommaAndVarName =
          Lexer::getSourceText(
              CharSourceRange::getTokenRange(AfterCommaToVarNameRange), SM,
              LangOpts)
              .ltrim(); // may be &, *, etc.

      // Check for preprocessor directive and add appropriate newline.
      if (AnyTokenBetweenCommaAndVarName.front() == '#')
        AnyTokenBetweenCommaAndVarName.insert(0, "\n");

      if (IsTagDef && (It == SecondElement))
        Diag << FixItHint::CreateInsertion(
            CommaLocation.getLocWithOffset(1),
            ";"); // Insert ; after closing }-brace
      else
        Diag << FixItHint::CreateReplacement(CommaLocation, ";");

      Diag << FixItHint::CreateReplacement(AfterCommaToVarNameRange,
                                           (llvm::Twine("\n") + CurrentIndent +
                                            UserWrittenType + " " +
                                            AnyTokenBetweenCommaAndVarName)
                                               .str());
    }
  }
}

std::string
OneNamePerDeclarationCheck::getUserWrittenType(const DeclStmt *DeclStmt,
                                               SourceManager &SM) {
  const auto FirstVarIt = DeclStmt->getDeclGroup().begin();

  if (const auto *TagDef = dyn_cast<TagDecl>(*FirstVarIt)) {
    auto TypeString = TagDef->getNameAsString();

    if (!getLangOpts().CPlusPlus)
      TypeString = (TagDef->getKindName() + " " + TypeString)
                       .str(); // Add struct, enum or union before the typename

    if (isa<TypedefDecl>(*(FirstVarIt + 1)))
      return "typedef " + TypeString;

    return TypeString;
  }

  SourceLocation Location;
  const Type *StmtType = nullptr;
  if (const auto *FirstVar = dyn_cast<DeclaratorDecl>(*FirstVarIt)) {
    Location = FirstVar->getLocation();
    StmtType = FirstVar->getType().getTypePtr();
  } else if (const auto *FirstVar = dyn_cast<TypedefDecl>(*FirstVarIt)) {
    Location = FirstVar->getLocation();
    StmtType = FirstVar->getTypeSourceInfo()->getType().getTypePtr();
    // Typedefs on function pointers are covered into LValueReferenceType's
    while (StmtType->isLValueReferenceType()) {
      StmtType = StmtType->getPointeeType().getTypePtr();
    }
  } else {
    llvm_unreachable(
        "Declaration is neither a DeclaratorDecl nor a TypedefDecl");
  }

  const SourceRange FVLoc(DeclStmt->getLocStart(), Location);
  std::string UserWrittenType =
      Lexer::getSourceText(CharSourceRange::getCharRange(FVLoc), SM,
                           getLangOpts())
          .trim();

  UserWrittenType = removeMultiLineComments(UserWrittenType);

  // UserWrittenType might be and we want ->
  // const int S::* -> const int
  // const int *&   -> const int
  // long **        -> long
  size_t Pos = std::string::npos;

  if (StmtType->isPointerType() || StmtType->isArrayType() ||
      StmtType->isReferenceType() || StmtType->isFunctionPointerType() ||
      StmtType->isFunctionProtoType()) {
    Pos = UserWrittenType.find_first_of("&*(");
    if (Pos != std::string::npos) // Might be hidden behind typedef etc.
      UserWrittenType.erase(Pos);

    while (StmtType->isAnyPointerType() || StmtType->isArrayType())
      StmtType = StmtType->getPointeeOrArrayElementType();
  }

  if (StmtType->isMemberFunctionPointerType() &&
      (Pos = UserWrittenType.find("(")) && Pos != std::string::npos) {
    UserWrittenType.erase(Pos);
    return StringRef(UserWrittenType).trim();
  }

  if (StmtType->isMemberDataPointerType() &&
      (Pos = UserWrittenType.rfind("::")) && Pos != std::string::npos) {
    // User might have inserted additional whitespaces:
    // int   S  ::
    //      ^-  ^- needed positions
    UserWrittenType.erase(Pos);
    UserWrittenType = StringRef(UserWrittenType).trim();
    Pos = UserWrittenType.rfind(" ");
    UserWrittenType.erase(Pos);
  }

  return StringRef(UserWrittenType).trim();
}

static std::string removeMultiLineComments(std::string Str) {
  size_t Pos1 = Str.find("/*");
  while (Pos1 != std::string::npos) {
    const size_t Pos2 = Str.find("*/", Pos1 + 1);
    Str.erase(Pos1, Pos2 - Pos1 + 2);
    Pos1 = Str.find("/*");
  }

  Str = StringRef(Str).trim();
  return Str;
}

static std::string getCurrentLineIndent(SourceLocation Loc,
                                        const SourceManager &SM) {
  const std::pair<FileID, unsigned> V = SM.getDecomposedLoc(Loc);
  const FileID FID = V.first;
  const unsigned StartOffs = V.second;

  const StringRef MB = SM.getBufferData(FID);

  const unsigned LineNo = SM.getLineNumber(FID, StartOffs) - 1;
  const SrcMgr::ContentCache *Content =
      SM.getSLocEntry(FID).getFile().getContentCache();
  const unsigned LineOffs = Content->SourceLineCache[LineNo];

  // Find the whitespace at the start of the line.
  StringRef IndentSpace;
  size_t I = LineOffs;
  while (isWhitespace(MB[I]) && MB[I] != '\n')
    ++I;
  IndentSpace = MB.substr(LineOffs, I - LineOffs);

  return IndentSpace;
}

} // namespace readability
} // namespace tidy
} // namespace clang
