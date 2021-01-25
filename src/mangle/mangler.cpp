#include "mangle/mangler.h"
#include <sstream>
#include <string>
#include <memory>
#include "ir/value.h"
#include "ir/type.h"
#include "ir/function.h"
#include "message/report_abort.h"

namespace {
    inline void mangle_identifier(std::stringstream &ss, std::string i) {
        ss << i.size() << i;
    }

    class _Mangler : public IR::TypeVisitor, IR::DeclSymbolVisitor {
    public:
        _Mangler(std::stringstream &ss): ss(ss) {}

        void mangle(IR::Type const &ty) {
            ss << 'T';
            ty.type_accept(*this);
            ss << 't';
        }
        void mangle(IR::DeclSymbol const &ds) {
            ss << 'D';
            ds.declsym_accept(*this);
            ss << 'd';
        }

    private:
        std::stringstream &ss;

        void type_visit(IR::IntType const &ty) override {
            switch (ty.size) {
                case 8:  ss << (ty.is_signed ? 's' : 'u'); break;
                case 16: ss << (ty.is_signed ? 'r' : 'w'); break;
                case 32: ss << (ty.is_signed ? 'q' : 'x'); break;
                case 64: ss << (ty.is_signed ? 'p' : 'y'); break;
            }
        }
        void type_visit(IR::FloatType const &ty) override {
            ss << (ty.size == 32 ? 'f' : 'd');
        }
        void type_visit(IR::CharType const &ty) override {
            ss << 'c';
        }
        void type_visit(IR::BoolType const &ty) override {
            ss << 'b';
        }
        void type_visit(IR::FunctionType const &ty) override {
            ss << 'F';
            mangle(*ty.ret);
            for (NNPtr<IR::Type const> pty : ty.paramtys)
                mangle(*pty);
            ss << 'f';
        }
        void type_visit(IR::VoidType const &ty) override {
            ss << 'v';
        }
        void type_visit(IR::GenericIntType const &ty) override {
            ss << 'x';
        }
        void type_visit(IR::GenericFloatType const &ty) override {
            ss << 'f';
        }
        void type_visit(IR::PointerType const &ty) override {
            ss << 'P';
            mangle(*ty.ty);
            ss << 'p';
        }

        void declsym_visit(IR::Type const &ty) override {
            ss << 'T';
            mangle(ty);
            ss << 't';
        }
        void declsym_visit(IR::Module const &m) override {
            ss << 'M';
            identifier(m.name());
            ss << 'm';
        }

        void identifier(std::string const &i) {
            ss << i.size() << i;
        }
    };
}

static void mangle_function(std::stringstream &ss, IR::Function &f) {
    ss << "F";

    _Mangler t (ss);

    mangle_identifier(ss, f.name);
    for (NNPtr<IR::Type const> ty : f.ty->paramtys)
        t.mangle(*ty);

    ss << "f";
}

static std::string mangle_path(IR::Function &f) {
    std::stringstream ss;
    ss << "_ksl_";
    mangle_function(ss, f);
    return ss.str();
}

std::string Mangle::NameMangler::mangle_name(IR::Function &f) {
    return mangle_path(f);
}
