#include "ir/value.h"

Value::Value(Type *t, ASTNS::AST *ast): type(t), ast(ast) {}
Value::Value(): type(nullptr), ast(nullptr) {}
