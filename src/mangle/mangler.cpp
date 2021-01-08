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

    class _Mangler : public IR::TypeVisitor, IR::DeclSymbolVisitor {
    public:
        _Mangler(std::stringstream &ss): ss(ss) {}

        void mangle(IR::Type *ty) {
            ss << 'T';
            ty->type_accept(this);
            ss << 't';
        }
        void mangle(IR::DeclSymbol *ds) {
            ss << 'D';
            ds->declsym_accept(this);
            ss << 'd';
        }

    private:
        std::stringstream &ss;

        void type_visitIntType(IR::IntType *ty) override {
            switch (ty->size) {
                case 8:  ss << (ty->isSigned ? 's' : 'u'); break;
                case 16: ss << (ty->isSigned ? 'r' : 'w'); break;
                case 32: ss << (ty->isSigned ? 'q' : 'x'); break;
                case 64: ss << (ty->isSigned ? 'p' : 'y'); break;
            }
        }
        void type_visitFloatType(IR::FloatType *ty) override {
            ss << (ty->size == 32 ? 'f' : 'd');
        }
        void type_visitCharType(IR::CharType *ty) override {
            ss << 'c';
        }
        void type_visitBoolType(IR::BoolType *ty) override {
            ss << 'b';
        }
        void type_visitFunctionType(IR::FunctionType *ty) override {
            ss << 'F';
            mangle(ty->ret);
            for (IR::Type *pty : ty->paramtys)
                mangle(pty);
            ss << 'f';
        }
        void type_visitVoidType(IR::VoidType *ty) override {
            ss << 'v';
        }
        void type_visitGenericIntType(IR::GenericIntType *ty) override {
            ss << 'x';
        }
        void type_visitGenericFloatType(IR::GenericFloatType *ty) override {
            ss << 'f';
        }
        void type_visitPointerType(IR::PointerType *ty) override {
            ss << 'P';
            mangle(ty->ty);
            ss << 'p';
        }

        void declsym_visitType(IR::Type *ty) override {
            ss << 'T';
            mangle(ty);
            ss << 't';
        }
        void declsym_visitModule(IR::Module *m) override {
            ss << 'M';
            identifier(m->name());
            ss << 'm';
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
