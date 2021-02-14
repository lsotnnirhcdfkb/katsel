#pragma once

namespace IR {
    class Instruction;
    class Br;
    namespace Instrs {
        // INSTR FWD START
        class Copy;
        class Or;
        class And;
        class Not;
        class ICmpNE;
        class ICmpEQ;
        class ICmpLT;
        class ICmpGT;
        class ICmpLE;
        class ICmpGE;
        class IAdd;
        class ISub;
        class IMult;
        class IDiv;
        class IMod;
        class INeg;
        class FCmpNE;
        class FCmpEQ;
        class FCmpLT;
        class FCmpGT;
        class FCmpLE;
        class FCmpGE;
        class FAdd;
        class FSub;
        class FMult;
        class FDiv;
        class FMod;
        class FNeg;
        class BitXor;
        class BitOr;
        class BitAnd;
        class BitNot;
        class ShiftR;
        class ShiftL;
        class NoOpCast;
        class IntToInt;
        class IntToFloat;
        class FloatToFloat;
        class FloatToInt;
        class Call;
        class Addrof;
        class DerefPtr;
        class PtrArith;
        class Return;
        class GotoBr;
        class CondBr;
        // INSTR FWD END
    }
}
