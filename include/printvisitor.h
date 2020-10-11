#include "visitor.h"
#include "ast.h"

class PrintVisitor :
    public ExprVisitor,
    public DeclVisitor,
    public TypeVisitor,
    public StmtVisitor,
    public ProgramVisitor,
    public ParamVisitor
{

};
