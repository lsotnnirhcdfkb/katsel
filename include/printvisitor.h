#include "visitor.h"
#include "ast.h"

class PrintVisitor :
    public ExprVisitor,
    public DeclVisitor,
    public TypeVisitor,
    public StmtVisitor,
    public ProgramVisitor,
    public ParamVisitor,
    public ArgVisitor
{
public:
    virtual void visitBinaryExpr(ASTNS::BinaryExpr *a) override;
    virtual void visitTernaryExpr(ASTNS::TernaryExpr *a) override;
    virtual void visitUnaryExpr(ASTNS::UnaryExpr *a) override;
    virtual void visitPrimaryExpr(ASTNS::PrimaryExpr *a) override;
    virtual void visitCallExpr(ASTNS::CallExpr *a) override;
    virtual void visitLtoRVExpr(ASTNS::LtoRVExpr *a) override;
    virtual void visitFunctionDecl(ASTNS::FunctionDecl *a) override;
    virtual void visitGlobalVarDecl(ASTNS::GlobalVarDecl *a) override;
    virtual void visitBaseType(ASTNS::BaseType *a) override;
    virtual void visitBlockStmt(ASTNS::BlockStmt *a) override;
    virtual void visitExprStmt(ASTNS::ExprStmt *a) override;
    virtual void visitReturnStmt(ASTNS::ReturnStmt *a) override;
    virtual void visitVarStmt(ASTNS::VarStmt *a) override;
    virtual void visitProgram(ASTNS::Program *a) override;
    virtual void visitParam(ASTNS::Param *a) override;
    virtual void visitArg(ASTNS::Arg *a) override;

private:
    int indent;
    bool pindent;
    // short for print at indent
    void pai(std::string &s);
    void pai(std::string &&s);

    template <typename T>
    void pchild(std::string &&s, const T &a);
    void ptok(std::string &&s, Token &t);
    void phead(std::string &&s);
    void pclose();
};
