//===--- LexerUtils.cpp - clang-tidy---------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "LexerUtils.h"

namespace clang {
namespace tidy {
namespace utils {
namespace lexer {

Token getPreviousNonCommentToken(const ASTContext &Context,
                                 SourceLocation Location) {
  const auto &SourceManager = Context.getSourceManager();
  Token Token;
  Token.setKind(tok::unknown);
  Location = Location.getLocWithOffset(-1);
  auto StartOfFile =
      SourceManager.getLocForStartOfFile(SourceManager.getFileID(Location));
  while (Location != StartOfFile) {
    Location = Lexer::GetBeginningOfToken(Location, SourceManager,
                                          Context.getLangOpts());
    if (!Lexer::getRawToken(Location, Token, SourceManager,
                            Context.getLangOpts()) &&
        !Token.is(tok::comment)) {
      break;
    }
    Location = Location.getLocWithOffset(-1);
  }
  return Token;
}

SourceLocation findTokenAfterLocation(SourceLocation Loc, ASTContext &Ctx,
                                      bool IsDecl,
                                      std::vector<tok::TokenKind> tokens) {
  const auto &SM = Ctx.getSourceManager();

  if (Loc.isMacroID()) {
    if (!Lexer::isAtEndOfMacroExpansion(Loc, SM, Ctx.getLangOpts(), &Loc))
      return SourceLocation();
  }
  Loc = Lexer::getLocForEndOfToken(Loc, /*Offset=*/0, SM, Ctx.getLangOpts());

  // Break down the source location.
  std::pair<FileID, unsigned> locInfo = SM.getDecomposedLoc(Loc);

  // Try to load the file buffer.
  bool invalidTemp = false;
  StringRef file = SM.getBufferData(locInfo.first, &invalidTemp);
  if (invalidTemp)
    return SourceLocation();

  const char *tokenBegin = file.data() + locInfo.second;

  // Lex from the startSearch of the given location.
  Lexer lexer(SM.getLocForStartOfFile(locInfo.first), Ctx.getLangOpts(),
              file.begin(), tokenBegin, file.end());
  Token tok;
  lexer.LexFromRawLexer(tok);
  for (auto token : tokens) {
    if (tok.is(token))
      return tok.getLocation();
  }

  if (!IsDecl)
    return SourceLocation();
  // Declaration may be followed with other tokens; such as an __attribute,
  // before ending with a semicolon.
  return findTokenAfterLocation(tok.getLocation(), Ctx, /*IsDecl*/ true,
                                tokens);
}

} // namespace lexer
} // namespace utils
} // namespace tidy
} // namespace clang
