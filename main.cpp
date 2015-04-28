#include <string>
#include <memory>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;

static llvm::cl::OptionCategory ToolSampleCategory("Sample Category");

class VarDeclHandler : public MatchFinder::MatchCallback {
  public:
    VarDeclHandler() {}

    virtual void run(const MatchFinder::MatchResult& Result) {
      if(const DeclStmt* DS = Result.Nodes.getNodeAs<clang::DeclStmt>("declStmt")) {
        llvm::outs() << "Found declaration match: ";
        DS->dump();
      }
    }
};

class IfStmtHandler : public MatchFinder::MatchCallback {
public:
  IfStmtHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    // The matched 'if' statement was bound to 'ifStmt'.
    if (const IfStmt *IfS = Result.Nodes.getNodeAs<clang::IfStmt>("ifStmt")) {
      const Stmt *Then = IfS->getThen();
      Rewrite.InsertText(Then->getLocStart(), "// the 'if' part\n", true, true);

      if (const Stmt *Else = IfS->getElse()) {
        Rewrite.InsertText(Else->getLocStart(), "// the 'else' part\n", true,
                           true);
      }
    }
  }

private:
  Rewriter &Rewrite;
};

class IncrementForLoopHandler : public MatchFinder::MatchCallback {
public:
  IncrementForLoopHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    const VarDecl *IncVar = Result.Nodes.getNodeAs<VarDecl>("incVarName");
    Rewrite.InsertText(IncVar->getLocStart(), "/* increment */", true, true);
  }

private:
  Rewriter &Rewrite;
};

// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser. It registers a couple of matchers and runs them on
// the AST.
class My2ASTConsumer : public ASTConsumer {
public:
  My2ASTConsumer(Rewriter &R) : HandlerForIf(R), HandlerForFor(R) {
    // Add a simple matcher for finding 'if' statements.
    //Matcher.addMatcher(ifStmt().bind("ifStmt"), &HandlerForIf);
    Matcher.addMatcher(declStmt().bind("declStmt"), &HandlerForVarDecl);

    // Add a complex matcher for finding 'for' loops with an initializer set
    // to 0, < comparison in the codition and an increment. For example:
    //
    //  for (int i = 0; i < N; ++i)
/*    Matcher.addMatcher(*/
        //forStmt(hasLoopInit(declStmt(hasSingleDecl(
                    //varDecl(hasInitializer(integerLiteral(equals(0))))
                        //.bind("initVarName")))),
                //hasIncrement(unaryOperator(
                    //hasOperatorName("++"),
                    //hasUnaryOperand(declRefExpr(to(
                        //varDecl(hasType(isInteger())).bind("incVarName")))))),
                //hasCondition(binaryOperator(
                    //hasOperatorName("<"),
                    //hasLHS(ignoringParenImpCasts(declRefExpr(to(
                        //varDecl(hasType(isInteger())).bind("condVarName"))))),
                    //hasRHS(expr(hasType(isInteger())))))).bind("forLoop"),
        /*&HandlerForFor);*/
  }

  void HandleImplicitImportDecl(ImportDecl *D) override {
    D->dump();
    llvm::outs() << "lol";
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    // Run the matchers when we have the whole TU parsed.
    //Matcher.matchAST(Context);
  }

private:
  IfStmtHandler HandlerForIf;
  VarDeclHandler HandlerForVarDecl;
  IncrementForLoopHandler HandlerForFor;
  MatchFinder Matcher;
};



#include "clang/AST/RecursiveASTVisitor.h"

#include <sstream>




      // By implementing RecursiveASTVisitor, we can specify which AST nodes
      // we're interested in by overriding relevant methods.
      class MyASTVisitor : public RecursiveASTVisitor<MyASTVisitor> {
      public:
        MyASTVisitor(Rewriter &R) : TheRewriter(R) {}

        bool VisitStmt(Stmt *s) {
          // Only care about If statements.
          if (isa<IfStmt>(s)) {
//            IfStmt *IfStatement = cast<IfStmt>(s);
//            Stmt *Then = IfStatement->getThen();

//            TheRewriter.InsertText(Then->getLocStart(), "// the 'if' part\n", true,
//                                   true);

//            Stmt *Else = IfStatement->getElse();
//            if (Else)
//              TheRewriter.InsertText(Else->getLocStart(), "// the 'else' part\n",
//                                     true, true);
          }

          return true;
        }


        bool VisitCXXRecordDecl(CXXRecordDecl* const decl)
        {
            auto declared_type = decl->getTypeForDecl();

            // ...
            declared_type->dump();

            return true;
        }

        bool VisitCXXMethodDecl(CXXMethodDecl* const func)
        {
            auto return_type = func->getReturnType().getTypePtr();
            return_type->dump();

            auto canonical_type = return_type->getCanonicalTypeInternal().getTypePtr();
            canonical_type->dump();

            return true;
        }


        bool VisitFunctionDecl(FunctionDecl *f) {
          // Only function definitions (with bodies), not declarations.
          if (f->hasBody()) {
//            Stmt *FuncBody = f->getBody();

//            // Type name as string
//            QualType QT = f->getReturnType();
//            std::string TypeStr = QT.getAsString();

//            // Function name
//            DeclarationName DeclName = f->getNameInfo().getName();
//            std::string FuncName = DeclName.getAsString();

//            // Add comment before
//            std::stringstream SSBefore;
//            SSBefore << "// Begin function " << FuncName << " returning " << TypeStr
//                     << "\n";
//            SourceLocation ST = f->getSourceRange().getBegin();
//            TheRewriter.InsertText(ST, SSBefore.str(), true, true);

//            // And after
//            std::stringstream SSAfter;
//            SSAfter << "\n// End function " << FuncName;
//            ST = FuncBody->getLocEnd().getLocWithOffset(1);
//            TheRewriter.InsertText(ST, SSAfter.str(), true, true);
          }

          return true;
        }

      private:
        Rewriter &TheRewriter;
      };

      // Implementation of the ASTConsumer interface for reading an AST produced
      // by the Clang parser.
      class MyASTConsumer : public ASTConsumer {
      public:
        MyASTConsumer(Rewriter &R) : Visitor(R) {}

        // Override the method that gets called for each parsed top-level
        // declaration.
        bool HandleTopLevelDecl(DeclGroupRef DR) override {
          for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
            // Traverse the declaration using our AST visitor.
            Visitor.TraverseDecl(*b);
            //(*b)->dump();
          }
          return true;
        }

      private:
        MyASTVisitor Visitor;
      };

      // For each source file provided to the tool, a new FrontendAction is created.
      class MyFrontendAction : public ASTFrontendAction {
      public:
        MyFrontendAction() {}
        void EndSourceFileAction() override {
          SourceManager &SM = TheRewriter.getSourceMgr();
          llvm::errs() << "** EndSourceFileAction for: "
                       << SM.getFileEntryForID(SM.getMainFileID())->getName() << "\n";

          // Now emit the rewritten buffer.
          TheRewriter.getEditBuffer(SM.getMainFileID()).write(llvm::outs());
        }

        std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                       StringRef file) override {
          llvm::errs() << "** Creating AST consumer for: " << file << "\n";
          TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
          return llvm::make_unique<MyASTConsumer>(TheRewriter);
        }

      private:
        Rewriter TheRewriter;
      };




///// CUSTOM TESTS ////

class ASTAction : public ASTConsumer {
  public:
      ASTAction() = default;
};

class PPObserver : public PPCallbacks {
  public:
      PPObserver(const SourceManager& sm) : SM(sm) {}
        
        virtual void FileChanged (SourceLocation Loc, FileChangeReason  Reason,
                SrcMgr::CharacteristicKind FileType, FileID PrevFID = FileID()) {
              llvm::outs() << "Found a changefile for preprocessor " << Reason << " " << FileType << " " <<  '\n';
              llvm::outs() << SM.getBuffer(PrevFID)->getBuffer();
              llvm::outs() << "\n\n";
                }
    const SourceManager& SM;    
};

class PPAction : public ASTFrontendAction {
   protected:
       virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance& compiler,  // NOLINT
                                                    llvm::StringRef /* dummy */) {
                                                  
         std::unique_ptr<PPObserver> ppO(new PPObserver(compiler.getSourceManager()));    
         compiler.getPreprocessor().addPPCallbacks(std::move(ppO) );
                 //compiler.getPreprocessor().addCommentHandler(preprocessor_consumer);
         std::unique_ptr<ASTAction> aaA(new ASTAction());
                         return std::move(aaA);
                           }
                 };

//
// First and foremost, we need to track each and every include that happens processed by this file.
// Either if this is a .h or a .cpp EVERY #include must be tracked down along with WHAT was included
//

int main(int argc, const char **argv) {
  CommonOptionsParser op(argc, argv, ToolSampleCategory);
  ClangTool Tool(op.getCompilations(), op.getSourcePathList());

  //return Tool.run(newFrontendActionFactory<PPAction>().get());
  return Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
}

