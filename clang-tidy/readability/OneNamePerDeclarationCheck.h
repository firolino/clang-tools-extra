//===--- OneNamePerDeclarationCheck.h - clang-tidy---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_ONE_NAME_PER_DECLARATION_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_ONE_NAME_PER_DECLARATION_H

#include "../ClangTidy.h"
#include <string>

namespace clang {
namespace tidy {
namespace readability {

/// Checks for declarations, declaring more than one name.
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/readability-one-name-per-declaration.html
class OneNamePerDeclarationCheck : public ClangTidyCheck {
private:
  std::string getUserWrittenType(const clang::DeclStmt *DeclStmt,
                                 SourceManager &SM);

public:
  OneNamePerDeclarationCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
};

} // namespace readability
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_ONE_NAME_PER_DECLARATION_H
