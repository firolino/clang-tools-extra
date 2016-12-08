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

SourceLocation findLocationAfterToken(SourceLocation Loc,
                                      const std::vector<tok::TokenKind> &Tokens,
                                      ASTContext &Context) {
  const auto &SM = Context.getSourceManager();
  const auto &LO = Context.getLangOpts();

  for (auto Token : Tokens) {
    auto FLoc = Lexer::findLocationAfterToken(
        Loc, Token, SM, LO, /*SkipTrailingWhitespaceAndNewLine*/ true);

    if (FLoc.isValid())
      return FLoc;
  }

  return SourceLocation();
}

} // namespace lexer
} // namespace utils
} // namespace tidy
} // namespace clang
