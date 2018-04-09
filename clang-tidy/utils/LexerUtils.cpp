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

Token getPreviousToken(const ASTContext &Context, SourceLocation Location,
                       bool SkipComments) {
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
        (!SkipComments || !Token.is(tok::comment))) {
      break;
    }
    Location = Location.getLocWithOffset(-1);
  }
  return Token;
}

SourceLocation findTokenLocationBackward(const ASTContext &Context,
                                         SourceLocation Location,
                                         tok::TokenKind TokenToFind) {
  const auto &SM = Context.getSourceManager();
  const auto &LO = Context.getLangOpts();
  auto StartOfFile = SM.getLocForStartOfFile(SM.getFileID(Location));

  Token CurrentToken;
  CurrentToken.setKind(tok::unknown);

  while (Location != StartOfFile) {
    if (!Lexer::getRawToken(Location, CurrentToken, SM, LO) &&
        CurrentToken.is(TokenToFind)) {
      return Location;
    }
    Location = Location.getLocWithOffset(-1);
  }

  return SourceLocation();
}

} // namespace lexer
} // namespace utils
} // namespace tidy
} // namespace clang
