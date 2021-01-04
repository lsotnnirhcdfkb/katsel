#include "mangle/mangler.h"
#include <sstream>
#include <string>
#include <memory>
#include "ir/value.h"
#include "ir/type.h"
#include "message/reportAbort.h"

namespace {
    inline void mangleIdentifier(std::stringstream &ss, std::string i) {
        ss << i.size() << i;
    }

    class _Mangler : public IR::TypeVisitor {
    public:
        _Mangler(std::stringstream &ss): ss(ss) {}

        void mangle(IR::Type *ty) {
            ss << "T";
            ty->accept(this);
            ss << "t";
        }

    private:
        std::stringstream &ss;
        void visitIntType(IR::IntType *ty) override {
            switch (ty->size) {
                case 8:  ss << (ty->isSigned ? 's' : 'u'); break;
                case 16: ss << (ty->isSigned ? 'r' : 'w'); break;
                case 32: ss << (ty->isSigned ? 'q' : 'x'); break;
                case 64: ss << (ty->isSigned ? 'p' : 'y'); break;
            }
        }
        void visitFloatType(IR::FloatType *ty) override {
            ss << (ty->size == 32 ? 'f' : 'd');
        }
        void visitCharType(IR::CharType *ty) override {
            ss << 'c';
        }
        void visitBoolType(IR::BoolType *ty) override {
            ss << 'b';
        }
        void visitFunctionType(IR::FunctionType *ty) override {
            ss << 'F';
            mangle(ty->ret);
            for (IR::Type *pty : ty->paramtys)
                mangle(pty);
            ss << 'f';
        }
        void visitVoidType(IR::VoidType *ty) override {
            ss << 'v';
        }
        void visitGenericIntType(IR::GenericIntType *ty) override {
            ss << 'x';
        }
        void visitGenericFloatType(IR::GenericFloatType *ty) override {
            ss << 'f';
        }
        void visitPointerType(IR::PointerType *ty) override {
            ss << 'P';
            mangle(ty->ty);
            ss << 'p';
        }

        void identifier(std::string const &i) {
            ss << i.size() << i;
        }
    };
}

static void mangleFunction(std::stringstream &ss, IR::Function &f) {
    ss << "F";

    _Mangler t (ss);

    mangleIdentifier(ss, f.name);
    for (IR::Type *ty : f.ty->paramtys)
        t.mangle(ty);

    ss << "f";
}

static std::string manglePath(IR::Function &f) {
    std::stringstream ss;
    ss << "_ksl_";
    mangleFunction(ss, f);
    return ss.str();
}

std::string Mangle::NameMangler::mangleName(IR::Function &f) {
    return manglePath(f);
}
