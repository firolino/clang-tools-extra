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

SourceLocation findTokenAfterLocation(SourceLocation Loc, ASTContext &Context,
                                      bool IsDecl,
                                      const std::vector<tok::TokenKind> &Tokens) {
  const auto &SM = Context.getSourceManager();

  if (Loc.isMacroID()) {
    if (!Lexer::isAtEndOfMacroExpansion(Loc, SM, Context.getLangOpts(), &Loc))
      return SourceLocation();
  }
  Loc = Lexer::getLocForEndOfToken(Loc, /*Offset=*/0, SM, Context.getLangOpts());

  // Break down the source location.
  std::pair<FileID, unsigned> LocInfo = SM.getDecomposedLoc(Loc);

  // Try to load the file buffer.
  bool InvalidTemp = false;
  StringRef file = SM.getBufferData(LocInfo.first, &InvalidTemp);
  if (InvalidTemp)
    return SourceLocation();

  const char *TokenBegin = file.data() + LocInfo.second;

  // Lex from the startSearch of the given location.
  Lexer Lexer(SM.getLocForStartOfFile(LocInfo.first), Context.getLangOpts(),
              file.begin(), TokenBegin, file.end());
  Token Tok;
  Lexer.LexFromRawLexer(Tok);
  for (auto token : Tokens) {
    if (Tok.is(token))
      return Tok.getLocation();
  }

  if (!IsDecl)
    return SourceLocation();
  // Declaration may be followed with other Tokens; such as an __attribute,
  // before ending with a semicolon.
  return findTokenAfterLocation(Tok.getLocation(), Context, /*IsDecl*/ true,
                                Tokens);
}

} // namespace lexer
} // namespace utils
} // namespace tidy
} // namespace clang
