//===--- LexerUtils.h - clang-tidy-------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_UTILS_LEXER_UTILS_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_UTILS_LEXER_UTILS_H

#include "clang/AST/ASTContext.h"
#include "clang/Lex/Lexer.h"
#include <vector>

namespace clang {
namespace tidy {
namespace utils {
namespace lexer {

/// Returns previous non-comment token skipping over any comment text or
/// ``tok::unknown`` if not found.
Token getPreviousNonCommentToken(const ASTContext &Context,
                                 SourceLocation Location);

/// \brief \arg Loc is the end of a statement range. This returns the location
/// immediately
/// after one of the token given in tokens is found after the statement.
/// If non of the tokens are found, the returned source location will be
/// invalid.
SourceLocation findLocationAfterToken(SourceLocation Loc,
                                      const std::vector<tok::TokenKind> &Tokens,
                                      ASTContext &Context);

} // namespace lexer
} // namespace utils
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_UTILS_LEXER_UTILS_H
