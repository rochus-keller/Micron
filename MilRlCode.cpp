/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Micron language project.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include "MilRlCode.h"
#include <QTextStream>
#include <QtDebug>
#include <algorithm>
#include <climits>

using namespace Mil;
using namespace Mil::Vm;
using namespace Mil::Rl;

static inline quint16 stackAlign(quint32 size, quint8 align)
{
    return (quint16)((size + align - 1) & ~(align - 1));
}

Rl::Code::Code(Vm::Code& c, quint8 stackAlignment)
    : d_code(c), d_stackAlign(stackAlignment)
{
}

bool Rl::Code::compile()
{
    d_procs.resize(d_code.procCount());
    for (int i = 0; i < d_code.procCount(); i++)
    {
        d_procs[i].llProc = d_code.getProc(i);
        if (!d_procs[i].llProc->external && !d_procs[i].llProc->ops.empty())
        {
            if (!compileProc(i))
                return false;
        }
    }
    return true;
}

struct LowerState {
    struct Slot {
        quint16 vreg;
        quint16 size;
    };

    std::vector<Slot> vstack;
    quint16 nextVreg;
    Rl::Procedure* mrl;
    Vm::Procedure* proc;
    std::vector<int> ll2mrl;
    std::vector<std::pair<int,int> > branchFixups;
    QSet<int> branchTargetSet;
    QMap<int, std::vector<Slot> > savedStacks;
    bool stackValid;
    quint16 SA;
    quint16 SA2;

    Vm::Code* code;

    LowerState(Rl::Procedure* m, Vm::Procedure* p, quint8 stackAlign, Vm::Code* c = 0)
        : nextVreg(0), mrl(m), proc(p),
          ll2mrl(p->ops.size() + 1, -1),
          stackValid(true),
          SA(stackAlign), SA2(2 * stackAlign),
          code(c)
    {}

    quint16 allocR(quint16 /*size*/) { return nextVreg++; }

    void pushR(quint16 r, quint16 sz)
    {
        Slot s; s.vreg = r; s.size = sz;
        vstack.push_back(s);
    }

    Slot popR()
    {
        Q_ASSERT(!vstack.empty());
        Slot s = vstack.back();
        vstack.pop_back();
        return s;
    }

    std::vector<Slot> popBytes(quint32 totalBytes)
    {
        std::vector<Slot> result;
        quint32 popped = 0;
        while (popped < totalBytes && !vstack.empty()) {
            Slot s = vstack.back();
            vstack.pop_back();
            result.push_back(s);
            popped += s.size;
        }
        return result;
    }

    int emitOp(quint16 op, quint16 r1 = NO_REG, quint16 r2 = NO_REG,
               quint16 r3 = NO_REG, quint32 aux = 0, quint32 aux2 = 0, bool templ = false)
    {
        int idx = mrl->ops.size();
        mrl->ops.push_back(Rl::Operation(op, r1, r2, r3, aux, aux2, templ));
        return idx;
    }

    int branchTarget(int pc) const
    {
        const Vm::Operation& o = proc->ops[pc];
        return pc + 1 + (o.minus ? -1 : 1) * (int)o.val;
    }

    void handleCall(quint16 callOp, quint32 argsSize, quint32 returnSize,
                    quint32 aux, quint32 aux2 = 0, quint16 extraReg = NO_REG)
    {
        std::vector<Slot> args = popBytes(argsSize);
        std::reverse(args.begin(), args.end());
        quint32 argOff = 0;
        for (size_t i = 0; i < args.size(); i++)
        {
            emitOp(RL_arg, args[i].vreg, NO_REG, NO_REG, argOff);
            argOff += args[i].size;
        }
        quint16 retReg = NO_REG;
        if (returnSize > 0)
            retReg = allocR(stackAlign(returnSize, SA));
        emitOp(callOp, retReg, extraReg, NO_REG, aux, aux2);
        if (returnSize > 0)
            pushR(retReg, stackAlign(returnSize, SA));
    }

    void reconcileStack(const std::vector<Slot>& saved)
    {
        if (saved.size() == vstack.size())
        {
            for (size_t i = 0; i < saved.size(); i++)
            {
                if (saved[i].vreg != vstack[i].vreg)
                    emitOp(RL_mov, saved[i].vreg, vstack[i].vreg);
            }
        }
    }

    void saveOrReconcile(int targetPc)
    {
        if (!savedStacks.contains(targetPc))
            savedStacks[targetPc] = vstack;
        else
        {
            const std::vector<Slot>& saved = savedStacks[targetPc];
            reconcileStack(saved);
        }
    }

    void prescanBranchTargets()
    {
        for (int i = 0; i < (int)proc->ops.size(); i++)
        {
            if (proc->ops[i].op == LL_br || proc->ops[i].op == LL_brfalse_i4)
            {
                const Vm::Operation& bop = proc->ops[i];
                int target = i + 1 + (bop.minus ? -1 : 1) * (int)bop.val;
                branchTargetSet.insert(target);
            }
        }
    }
};

bool Rl::Code::compileProc(int procIdx)
{
    Vm::Procedure* proc = d_code.getProc(procIdx);
    Procedure& mrl = d_procs[procIdx];
    mrl.llProc = proc;

    if (proc->external || proc->ops.empty())
        return true;

    LowerState L(&mrl, proc, d_stackAlign, &d_code);
    L.prescanBranchTargets();

    int pc = 0;
    while (pc < (int)proc->ops.size())
    {
        // At branch targets: restore saved stack state
        if (L.branchTargetSet.contains(pc))
        {
            if (L.savedStacks.contains(pc))
            {
                if (L.stackValid)
                    L.reconcileStack(L.savedStacks[pc]);
                L.vstack = L.savedStacks[pc];
            }
            else if (L.stackValid)
                L.savedStacks[pc] = L.vstack;
            L.stackValid = true;
        }

        if (!L.stackValid)
        {
            // After unconditional br, skip dead code until next branch target
            L.ll2mrl[pc] = L.mrl->ops.size();
            pc++;
            continue;
        }

        L.ll2mrl[pc] = L.mrl->ops.size();

        const Vm::Operation& op = proc->ops[pc];

        switch ((LL_op)op.op)
        {
        case LL_invalid:
            pc++;
            break;

        case LL_add_i4: case LL_add_i8: case LL_add_r4: case LL_add_r8:
        case LL_sub_i4: case LL_sub_i8: case LL_sub_r4: case LL_sub_r8:
        case LL_mul_i4: case LL_mul_i8: case LL_mul_r4: case LL_mul_r8:
        case LL_div_i4: case LL_div_i8: case LL_div_r4: case LL_div_r8:
        case LL_div_un_i4: case LL_div_un_i8:
        case LL_rem_i4: case LL_rem_i8: case LL_rem_un_i4: case LL_rem_un_i8:
        case LL_and_i4: case LL_and_i8: case LL_or_i4: case LL_or_i8:
        case LL_xor_i4: case LL_xor_i8:
        case LL_shl_i4: case LL_shl_i8: case LL_shr_i4: case LL_shr_i8:
        case LL_shr_un_i4: case LL_shr_un_i8:
        {
            LowerState::Slot b = L.popR();
            LowerState::Slot a = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, a.vreg, b.vreg);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_ceq_i4: case LL_ceq_i8: case LL_ceq_r4: case LL_ceq_r8:
        case LL_ceq_p: case LL_ceq_pp:
        case LL_cgt_i4: case LL_cgt_i8: case LL_cgt_r4: case LL_cgt_r8:
        case LL_cgt_u4: case LL_cgt_u8: case LL_cgt_p:
        case LL_clt_i4: case LL_clt_i8: case LL_clt_r4: case LL_clt_r8:
        case LL_clt_u4: case LL_clt_u8: case LL_clt_p:
        {
            LowerState::Slot b = L.popR();
            LowerState::Slot a = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, a.vreg, b.vreg);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_abs_i4: case LL_abs_i8: case LL_abs_r4: case LL_abs_r8:
        case LL_neg_i4: case LL_neg_i8: case LL_neg_r4: case LL_neg_r8:
        case LL_not_i4: case LL_not_i8:
        case LL_conv_i1_i4: case LL_conv_i1_i8: case LL_conv_i1_r4: case LL_conv_i1_r8:
        case LL_conv_i2_i4: case LL_conv_i2_i8: case LL_conv_i2_r4: case LL_conv_i2_r8:
        case LL_conv_i4_i8: case LL_conv_i4_r4: case LL_conv_i4_r8:
        case LL_conv_i8_i4: case LL_conv_i8_r4: case LL_conv_i8_r8:
        case LL_conv_u1_i4: case LL_conv_u1_i8: case LL_conv_u1_r4: case LL_conv_u1_r8:
        case LL_conv_u2_i4: case LL_conv_u2_i8: case LL_conv_u2_r4: case LL_conv_u2_r8:
        case LL_conv_u4_i8: case LL_conv_u4_r4: case LL_conv_u4_r8:
        case LL_conv_u8_i4: case LL_conv_u8_r4: case LL_conv_u8_r8:
        case LL_conv_r4_i4: case LL_conv_r4_i8: case LL_conv_r4_r8:
        case LL_conv_r8_i4: case LL_conv_r8_i8: case LL_conv_r8_r4:
        {
            LowerState::Slot a = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, a.vreg);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_ldloc_i1: case LL_ldloc_i2: case LL_ldloc_i4: case LL_ldloc_i8:
        case LL_ldloc_u1: case LL_ldloc_u2: case LL_ldloc_u4: case LL_ldloc_u8:
        case LL_ldloc_r4: case LL_ldloc_r8: case LL_ldloc_p:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldloc_pp:
        {
            quint16 d = L.allocR(L.SA2);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA2);
            pc++;
            break;
        }
        case LL_ldloc_vt:
        {
            Q_ASSERT(pc + 1 < (int)proc->ops.size() && proc->ops[pc+1].op == LL_vt_size);
            quint16 vtSz = stackAlign(proc->ops[pc+1].val, L.SA);
            quint16 d = L.allocR(vtSz);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val, proc->ops[pc+1].val);
            L.pushR(d, vtSz);
            L.ll2mrl[pc+1] = L.mrl->ops.size(); // map vt_size to next MRL op
            pc += 2;
            break;
        }
        case LL_ldloca:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_stloc_i1: case LL_stloc_i2: case LL_stloc_i4: case LL_stloc_i8:
        case LL_stloc_r4: case LL_stloc_r8: case LL_stloc_p: case LL_stloc_pp:
        {
            LowerState::Slot a = L.popR();
            L.emitOp(op.op, a.vreg, NO_REG, NO_REG, op.val);
            pc++;
            break;
        }
        case LL_stloc_vt:
        {
            Q_ASSERT(pc + 1 < (int)proc->ops.size() && proc->ops[pc+1].op == LL_vt_size);
            LowerState::Slot a = L.popR();
            L.emitOp(op.op, a.vreg, NO_REG, NO_REG, op.val, proc->ops[pc+1].val);
            L.ll2mrl[pc+1] = L.mrl->ops.size();
            pc += 2;
            break;
        }

        case LL_ldarg_i1: case LL_ldarg_i2: case LL_ldarg_i4: case LL_ldarg_i8:
        case LL_ldarg_u1: case LL_ldarg_u2: case LL_ldarg_u4: case LL_ldarg_u8:
        case LL_ldarg_r4: case LL_ldarg_r8: case LL_ldarg_p:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldarg_pp:
        {
            quint16 d = L.allocR(L.SA2);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA2);
            pc++;
            break;
        }
        case LL_ldarg_vt:
        {
            Q_ASSERT(pc + 1 < (int)proc->ops.size() && proc->ops[pc+1].op == LL_vt_size);
            quint16 vtSz = stackAlign(proc->ops[pc+1].val, L.SA);
            quint16 d = L.allocR(vtSz);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val, proc->ops[pc+1].val);
            L.pushR(d, vtSz);
            L.ll2mrl[pc+1] = L.mrl->ops.size();
            pc += 2;
            break;
        }
        case LL_ldarga:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_starg_i1: case LL_starg_i2: case LL_starg_i4: case LL_starg_i8:
        case LL_starg_r4: case LL_starg_r8: case LL_starg_p: case LL_starg_pp:
        {
            LowerState::Slot a = L.popR();
            L.emitOp(op.op, a.vreg, NO_REG, NO_REG, op.val);
            pc++;
            break;
        }
        case LL_starg_vt:
        {
            Q_ASSERT(pc + 1 < (int)proc->ops.size() && proc->ops[pc+1].op == LL_vt_size);
            LowerState::Slot a = L.popR();
            L.emitOp(op.op, a.vreg, NO_REG, NO_REG, op.val, proc->ops[pc+1].val);
            L.ll2mrl[pc+1] = L.mrl->ops.size();
            pc += 2;
            break;
        }

        case LL_ldvar_i1: case LL_ldvar_i2: case LL_ldvar_i4: case LL_ldvar_i8:
        case LL_ldvar_u1: case LL_ldvar_u2: case LL_ldvar_u4: case LL_ldvar_u8:
        case LL_ldvar_r4: case LL_ldvar_r8: case LL_ldvar_p:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldvar_pp:
        {
            quint16 d = L.allocR(L.SA2);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA2);
            pc++;
            break;
        }
        case LL_ldvar_vt:
        {
            Q_ASSERT(pc + 1 < (int)proc->ops.size() && proc->ops[pc+1].op == LL_vt_size);
            quint16 vtSz = stackAlign(proc->ops[pc+1].val, L.SA);
            quint16 d = L.allocR(vtSz);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val, proc->ops[pc+1].val);
            L.pushR(d, vtSz);
            L.ll2mrl[pc+1] = L.mrl->ops.size();
            pc += 2;
            break;
        }
        case LL_ldvara:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_stvar_i1: case LL_stvar_i2: case LL_stvar_i4: case LL_stvar_i8:
        case LL_stvar_r4: case LL_stvar_r8: case LL_stvar_p: case LL_stvar_pp:
        {
            LowerState::Slot a = L.popR();
            L.emitOp(op.op, a.vreg, NO_REG, NO_REG, op.val);
            pc++;
            break;
        }
        case LL_stvar_vt:
        {
            Q_ASSERT(pc + 1 < (int)proc->ops.size() && proc->ops[pc+1].op == LL_vt_size);
            LowerState::Slot a = L.popR();
            L.emitOp(op.op, a.vreg, NO_REG, NO_REG, op.val, proc->ops[pc+1].val);
            L.ll2mrl[pc+1] = L.mrl->ops.size();
            pc += 2;
            break;
        }

        case LL_ldfld_i1: case LL_ldfld_i2: case LL_ldfld_i4: case LL_ldfld_i8:
        case LL_ldfld_u1: case LL_ldfld_u2: case LL_ldfld_u4: case LL_ldfld_u8:
        case LL_ldfld_r4: case LL_ldfld_r8: case LL_ldfld_p:
        {
            LowerState::Slot obj = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, obj.vreg, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldfld_pp:
        {
            LowerState::Slot obj = L.popR();
            quint16 d = L.allocR(L.SA2);
            L.emitOp(op.op, d, obj.vreg, NO_REG, op.val);
            L.pushR(d, L.SA2);
            pc++;
            break;
        }
        case LL_ldfld_vt:
        {
            Q_ASSERT(pc + 1 < (int)proc->ops.size() && proc->ops[pc+1].op == LL_vt_size);
            quint16 vtSz = stackAlign(proc->ops[pc+1].val, L.SA);
            LowerState::Slot obj = L.popR();
            quint16 d = L.allocR(vtSz);
            L.emitOp(op.op, d, obj.vreg, NO_REG, op.val, proc->ops[pc+1].val);
            L.pushR(d, vtSz);
            L.ll2mrl[pc+1] = L.mrl->ops.size();
            pc += 2;
            break;
        }
        case LL_ldflda:
        {
            LowerState::Slot obj = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, obj.vreg, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_stfld_i1: case LL_stfld_i2: case LL_stfld_i4: case LL_stfld_i8:
        case LL_stfld_r4: case LL_stfld_r8: case LL_stfld_p: case LL_stfld_pp:
        {
            LowerState::Slot val = L.popR();
            LowerState::Slot obj = L.popR();
            L.emitOp(op.op, obj.vreg, val.vreg, NO_REG, op.val);
            pc++;
            break;
        }
        case LL_stfld_vt:
        {
            Q_ASSERT(pc + 1 < (int)proc->ops.size() && proc->ops[pc+1].op == LL_vt_size);
            LowerState::Slot val = L.popR();
            LowerState::Slot obj = L.popR();
            L.emitOp(op.op, obj.vreg, val.vreg, NO_REG, op.val, proc->ops[pc+1].val);
            L.ll2mrl[pc+1] = L.mrl->ops.size();
            pc += 2;
            break;
        }

        case LL_ldelem_i1: case LL_ldelem_i2: case LL_ldelem_i4: case LL_ldelem_i8:
        case LL_ldelem_u1: case LL_ldelem_u2: case LL_ldelem_u4: case LL_ldelem_u8:
        case LL_ldelem_r4: case LL_ldelem_r8: case LL_ldelem_p:
        {
            LowerState::Slot idx = L.popR();
            LowerState::Slot arr = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, arr.vreg, idx.vreg);
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldelem_vt:
        {
            // SizeArg: val = element byte size
            quint16 vtSz = stackAlign(op.val, L.SA);
            LowerState::Slot idx = L.popR();
            LowerState::Slot arr = L.popR();
            quint16 d = L.allocR(vtSz);
            L.emitOp(op.op, d, arr.vreg, idx.vreg, op.val);
            L.pushR(d, vtSz);
            pc++;
            break;
        }
        case LL_ldelema:
        {
            // pop index, pop array ptr, push address; val=element size
            LowerState::Slot idx = L.popR();
            LowerState::Slot arr = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, arr.vreg, idx.vreg, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_stelem_i1: case LL_stelem_i2: case LL_stelem_i4: case LL_stelem_i8:
        case LL_stelem_r4: case LL_stelem_r8: case LL_stelem_p:
        {
            LowerState::Slot val = L.popR();
            LowerState::Slot idx = L.popR();
            LowerState::Slot arr = L.popR();
            L.emitOp(op.op, arr.vreg, idx.vreg, val.vreg);
            pc++;
            break;
        }
        case LL_stelem_vt:
        {
            // SizeArg: val = element byte size
            LowerState::Slot val = L.popR();
            LowerState::Slot idx = L.popR();
            LowerState::Slot arr = L.popR();
            L.emitOp(op.op, arr.vreg, idx.vreg, val.vreg, op.val);
            pc++;
            break;
        }

        case LL_ldind_i1: case LL_ldind_i2: case LL_ldind_i4: case LL_ldind_i8:
        case LL_ldind_u1: case LL_ldind_u2: case LL_ldind_u4: case LL_ldind_u8:
        case LL_ldind_r4: case LL_ldind_r8: case LL_ldind_p:
        {
            LowerState::Slot ptr = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, ptr.vreg);
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldind_vt:
        {
            // SizeArg: val = byte size
            quint16 vtSz = stackAlign(op.val, L.SA);
            LowerState::Slot ptr = L.popR();
            quint16 d = L.allocR(vtSz);
            L.emitOp(op.op, d, ptr.vreg, NO_REG, op.val, op.val);
            L.pushR(d, vtSz);
            pc++;
            break;
        }
        case LL_ldind_str:
        {
            // SizeArg: val = max string length (treat as value type)
            quint16 vtSz = stackAlign(op.val, L.SA);
            LowerState::Slot ptr = L.popR();
            quint16 d = L.allocR(vtSz);
            L.emitOp(op.op, d, ptr.vreg, NO_REG, op.val, op.val);
            L.pushR(d, vtSz);
            pc++;
            break;
        }

        case LL_stind_i1: case LL_stind_i2: case LL_stind_i4: case LL_stind_i8:
        case LL_stind_r4: case LL_stind_r8: case LL_stind_p:
        {
            LowerState::Slot val = L.popR();
            LowerState::Slot ptr = L.popR();
            L.emitOp(op.op, ptr.vreg, val.vreg);
            pc++;
            break;
        }
        case LL_stind_vt:
        {
            // SizeArg: val = byte size
            LowerState::Slot val = L.popR();
            LowerState::Slot ptr = L.popR();
            L.emitOp(op.op, ptr.vreg, val.vreg, NO_REG, op.val);
            pc++;
            break;
        }

        case LL_ldc_i4:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val); // val = int pool index
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldc_i8:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val); // val = int pool index
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldc_r4: case LL_ldc_r8:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val); // val = float pool index
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldc_i4_m1: case LL_ldc_i4_0: case LL_ldc_i4_1: case LL_ldc_i4_2:
        case LL_ldc_i4_3: case LL_ldc_i4_4: case LL_ldc_i4_5: case LL_ldc_i4_6:
        case LL_ldc_i4_7: case LL_ldc_i4_8:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_ldnull:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_ldstr:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val); // val = string pool index
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_ldobj:
        {
            // val = object pool index; get actual object size from pool
            quint16 objSz = L.SA;
            if (L.code) {
                const std::vector<char>& obj = L.code->getObject(op.val);
                objSz = stackAlign(obj.size(), L.SA);
            }
            quint16 d = L.allocR(objSz);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val, objSz);
            L.pushR(d, objSz);
            pc++;
            break;
        }

        case LL_br:
        {
            int target = L.branchTarget(pc);
            L.saveOrReconcile(target);
            int mrlPc = L.emitOp(op.op, NO_REG, NO_REG, NO_REG, target);
            L.branchFixups.push_back(std::make_pair(mrlPc, target));
            L.stackValid = false;
            pc++;
            break;
        }
        case LL_brfalse_i4:
        {
            LowerState::Slot cond = L.popR();
            int target = L.branchTarget(pc);
            L.saveOrReconcile(target);
            int mrlPc = L.emitOp(op.op, cond.vreg, NO_REG, NO_REG, target);
            L.branchFixups.push_back(std::make_pair(mrlPc, target));
            pc++;
            break;
        }

        case LL_ldproc:
        {
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val); // val = proc index
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_ldmeth:
        {
            // pop obj ptr (8), push MethRef (16)
            LowerState::Slot obj = L.popR();
            quint16 d = L.allocR(L.SA2);
            L.emitOp(op.op, d, obj.vreg, NO_REG, op.val); // val = vtable slot
            L.pushR(d, L.SA2);
            pc++;
            break;
        }
        case LL_ldmeth_struct:
        {
            // pop obj ptr (8), push MethRef (16)
            LowerState::Slot obj = L.popR();
            quint16 d = L.allocR(L.SA2);
            L.emitOp(op.op, d, obj.vreg, NO_REG, op.val); // val = proc index
            L.pushR(d, L.SA2);
            pc++;
            break;
        }
        case LL_ldmeth_iface:
        {
            // pop IfaceRef (16), push MethRef (16)
            LowerState::Slot iface = L.popR();
            quint16 d = L.allocR(L.SA2);
            L.emitOp(op.op, d, iface.vreg, NO_REG, op.val); // val = method slot
            L.pushR(d, L.SA2);
            pc++;
            break;
        }
        case LL_ldiface:
        {
            // pop obj ptr (8), push IfaceRef (16); val = vtable index
            LowerState::Slot obj = L.popR();
            quint16 d = L.allocR(L.SA2);
            L.emitOp(op.op, d, obj.vreg, NO_REG, op.val);
            L.pushR(d, L.SA2);
            pc++;
            break;
        }

        case LL_call:
        {
            Vm::Procedure* callee = d_code.getProc(op.val);
            L.handleCall(op.op, callee->argsSize, callee->returnSize, op.val);
            pc++;
            break;
        }
        case LL_callvirt:
        {
            Vm::Procedure* callee = d_code.getProc(op.val);
            L.handleCall(op.op, callee->argsSize, callee->returnSize, op.val);
            pc++;
            break;
        }
        case LL_callinst:
        {
            Vm::Procedure* callee = d_code.getProc(op.val);
            L.handleCall(op.op, callee->argsSize, callee->returnSize, op.val);
            pc++;
            break;
        }
        case LL_calli:
        {
            // Function pointer is on top of stack
            LowerState::Slot fptr = L.popR();
            // Decode argsSize and returnSize from val
            quint32 argsSize = op.val & 0x7FF;
            quint32 retSizeRaw = op.val >> 11;
            quint32 returnSize = retSizeRaw & ~1u; // clear fpReturn bit
            L.handleCall(op.op, argsSize, returnSize, op.val, 0, fptr.vreg);
            pc++;
            break;
        }
        case LL_callmi:
        {
            // MethRef is on top of stack
            LowerState::Slot methref = L.popR();
            // Decode argsSize and returnSize from val
            quint32 argsSize = op.val & 0x7FF;
            quint32 retSizeRaw = op.val >> 11;
            quint32 returnSize = retSizeRaw & ~1u;
            // argsSize includes self pointer; explicit args = argsSize - ptrSize
            quint32 explicitArgsSize = (argsSize > (quint32)L.SA) ? argsSize - L.SA : 0;
            L.handleCall(op.op, explicitArgsSize, returnSize, op.val, 0, methref.vreg);
            pc++;
            break;
        }

        case LL_ret:
        {
            // val = return size; return value is on stack
            if (!L.vstack.empty())
            {
                LowerState::Slot rv = L.popR();
                L.emitOp(op.op, rv.vreg, NO_REG, NO_REG, op.val);
            }
            else
                L.emitOp(op.op, NO_REG, NO_REG, NO_REG, op.val);
            pc++;
            break;
        }
        case LL_ret_void:
        {
            L.emitOp(op.op);
            pc++;
            break;
        }

        case LL_pop:
        {
            // Discard top value(s) totalling op.val bytes
            quint32 remaining = stackAlign(op.val, L.SA);
            while (remaining > 0 && !L.vstack.empty())
            {
                LowerState::Slot s = L.popR();
                remaining = (remaining >= s.size) ? remaining - s.size : 0;
            }
            pc++;
            break;
        }
        case LL_dup:
        {
            // Duplicate the top op.val bytes
            quint32 dupSize = stackAlign(op.val, L.SA);
            // Collect slots from top that cover dupSize bytes
            std::vector<LowerState::Slot> toDup;
            quint32 covered = 0;
            for (int i = (int)L.vstack.size() - 1; i >= 0 && covered < dupSize; i--)
            {
                toDup.push_back(L.vstack[i]);
                covered += L.vstack[i].size;
            }
            // Duplicate in original order (bottom first)
            std::reverse(toDup.begin(), toDup.end());
            for (size_t i = 0; i < toDup.size(); i++)
            {
                quint16 d = L.allocR(toDup[i].size);
                L.emitOp(RL_mov, d, toDup[i].vreg);
                L.pushR(d, toDup[i].size);
            }
            pc++;
            break;
        }

        case LL_alloc1: case LL_calloc1: case LL_gcalloc1:
        {
            // push 1 pointer
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val, 0, op.minus);
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_allocN: case LL_callocN: case LL_gcallocN:
        {
            // pop count (i4), push pointer
            LowerState::Slot count = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, count.vreg, NO_REG, op.val, 0, op.minus);
            L.pushR(d, L.SA);
            pc++;
            break;
        }
        case LL_free:
        {
            LowerState::Slot ptr = L.popR();
            L.emitOp(op.op, ptr.vreg);
            pc++;
            break;
        }
        case LL_initobj:
        {
            LowerState::Slot ptr = L.popR();
            L.emitOp(op.op, ptr.vreg, NO_REG, NO_REG, op.val, 0, op.minus);
            pc++;
            break;
        }

        case LL_strcpy:
        {
            LowerState::Slot rhs = L.popR();
            LowerState::Slot lhs = L.popR();
            L.emitOp(op.op, lhs.vreg, rhs.vreg, NO_REG, op.val);
            pc++;
            break;
        }

        case LL_isinst:
        {
            LowerState::Slot obj = L.popR();
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, obj.vreg, NO_REG, op.val); // val = vtable index
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_line:
        {
            L.emitOp(op.op, NO_REG, NO_REG, NO_REG, op.val); // val = line number
            pc++;
            break;
        }
        case LL_nop:
        case LL_cli:
        case LL_sti:
        {
            L.emitOp(op.op);
            pc++;
            break;
        }

        case LL_putreg:
        {
            // pop 1 value, write to HW register
            LowerState::Slot val = L.popR();
            L.emitOp(op.op, val.vreg, NO_REG, NO_REG, op.val);
            pc++;
            break;
        }
        case LL_getreg:
        {
            // push 1 value from HW register
            quint16 d = L.allocR(L.SA);
            L.emitOp(op.op, d, NO_REG, NO_REG, op.val);
            L.pushR(d, L.SA);
            pc++;
            break;
        }

        case LL_vt_size:
        {
            // standalone vt_size — should not happen in well-formed LL
            qWarning() << "MRL: unexpected standalone vt_size at LL PC" << pc;
            pc++;
            break;
        }

        case LL_sizeof:
        case LL_newvla:
        {
            qWarning() << "MRL: unimplemented LL op" << Vm::Code::op_names[op.op]
                        << "at PC" << pc;
            pc++;
            break;
        }

        default:
        {
            qCritical() << "MRL: unknown LL op" << op.op << "at PC" << pc;
            return false;
        }
        }
    }

    // sentinel mapping
    L.ll2mrl[proc->ops.size()] = L.mrl->ops.size();

    // Fixup branch targets: convert absolute LL PCs to MRL PCs
    for (size_t i = 0; i < L.branchFixups.size(); i++)
    {
        int mrlPc = L.branchFixups[i].first;
        int llTarget = L.branchFixups[i].second;
        if (llTarget >= 0 && llTarget < (int)L.ll2mrl.size() && L.ll2mrl[llTarget] >= 0)
            mrl.ops[mrlPc].aux = L.ll2mrl[llTarget];
        else
        {
            qWarning() << "MRL: branch target fixup failed for LL PC" << llTarget
                        << "in proc" << proc->decl->toPath();
        }
    }

    L.mrl->numVregs = L.nextVreg;
    d_stats.procsLowered++;
    d_stats.totalVregs += L.nextVreg;
    d_stats.totalMrlOps += L.mrl->ops.size();
    d_stats.totalLlOps += proc->ops.size();

    return true;
}

bool Rl::Code::compactAll(int maxRegs)
{
    for (int i = 0; i < d_code.procCount(); i++)
    {
        if (!d_procs[i].llProc->external && !d_procs[i].llProc->ops.empty())
        {
            if (!compactRegisters(i, maxRegs))
                return false;
        }
    }
    return true;
}

bool Rl::Code::compactRegisters(int procIdx, int maxRegs)
{
    if (procIdx < 0 || procIdx >= (int)d_procs.size())
        return false;

    Procedure& proc = d_procs[procIdx];
    const int n = proc.numVregs;
    if (n <= 1 || proc.ops.empty())
        return true;

    // Phase 1: compute liveness intervals [start, end] per vreg
    struct Interval {
        int start;
        int end;
    };
    std::vector<Interval> intervals(n);
    for (int i = 0; i < n; i++)
    {
        intervals[i].start = INT_MAX;
        intervals[i].end = -1;
    }
    for (int pc = 0; pc < (int)proc.ops.size(); pc++)
    {
        const Operation& op = proc.ops[pc];
        const quint16 regs[3] = { op.r1, op.r2, op.r3 };
        for (int k = 0; k < 3; k++)
        {
            quint16 r = regs[k];
            if (r != NO_REG && r < (quint16)n)
            {
                if (pc < intervals[r].start) intervals[r].start = pc;
                if (pc > intervals[r].end) intervals[r].end = pc;
            }
        }
    }

    // Phase 2: sort live vregs by interval start (shell sort, C++03)
    std::vector<int> sorted;
    sorted.reserve(n);
    for (int i = 0; i < n; i++)
    {
        if (intervals[i].start != INT_MAX)
            sorted.push_back(i);
    }
    const int sn = (int)sorted.size();
    for (int gap = sn / 2; gap > 0; gap /= 2)
    {
        for (int i = gap; i < sn; i++)
        {
            int tmp = sorted[i];
            int j = i;
            for (; j >= gap && intervals[sorted[j - gap]].start > intervals[tmp].start; j -= gap)
                sorted[j] = sorted[j - gap];
            sorted[j] = tmp;
        }
    }

    // Phase 3: linear scan — assign compacted register numbers
    std::vector<quint16> remap(n, (quint16)NO_REG);
    std::vector<bool> regFree(maxRegs, true);
    struct Active { int vreg; int reg; int end; };
    std::vector<Active> active;
    int highWater = 0;

    for (int si = 0; si < sn; si++)
    {
        int v = sorted[si];

        // expire old intervals
        for (int ai = (int)active.size() - 1; ai >= 0; ai--)
        {
            if (active[ai].end < intervals[v].start)
            {
                regFree[active[ai].reg] = true;
                active.erase(active.begin() + ai);
            }
        }

        // find lowest free register
        int bestReg = -1;
        for (int ri = 0; ri < maxRegs; ri++)
        {
            if (regFree[ri]) { bestReg = ri; break; }
        }
        if (bestReg < 0)
        {
            qWarning() << "MRL compactRegisters: out of registers for proc" << procIdx
                        << "(need more than" << maxRegs << ")";
            return false;
        }

        remap[v] = (quint16)bestReg;
        regFree[bestReg] = false;
        Active a;
        a.vreg = v;
        a.reg = bestReg;
        a.end = intervals[v].end;
        active.push_back(a);
        if (bestReg + 1 > highWater)
            highWater = bestReg + 1;
    }

    // Phase 4: renumber all register operands in-place
    for (int pc = 0; pc < (int)proc.ops.size(); pc++)
    {
        Operation& op = proc.ops[pc];
        if (op.r1 != NO_REG && op.r1 < (quint16)n) op.r1 = remap[op.r1];
        if (op.r2 != NO_REG && op.r2 < (quint16)n) op.r2 = remap[op.r2];
        if (op.r3 != NO_REG && op.r3 < (quint16)n) op.r3 = remap[op.r3];
    }

    proc.numVregs = (quint16)highWater;
    return true;
}

const char* Rl::Code::opName(int op)
{
    switch (op) {
    case RL_mov: return "mov";
    case RL_arg: return "arg";
    default:      return "???";
    }
}

static void printReg(QTextStream& out, quint16 r)
{
    if (r == NO_REG)
        out << "_";
    else
        out << "v" << r;
}

bool Rl::Code::dumpProc(QTextStream& out, int procIdx)
{
    if (procIdx < 0 || procIdx >= (int)d_procs.size())
        return false;

    Procedure& mrl = d_procs[procIdx];
    if (!mrl.llProc || mrl.ops.empty())
        return false;

    out << "proc " << mrl.llProc->decl->toPath()
        << " (vregs: " << mrl.numVregs
        << ", ops: " << mrl.ops.size() << ")" << endl;

    // Collect branch targets for label emission
    QSet<int> labels;
    for (size_t i = 0; i < mrl.ops.size(); i++)
    {
        quint16 op = mrl.ops[i].op;
        if (op == LL_br || op == LL_brfalse_i4)
            labels.insert(mrl.ops[i].aux);
    }

    for (int pc = 0; pc < (int)mrl.ops.size(); pc++)
    {
        if (labels.contains(pc))
            out << "  L" << pc << ":" << endl;

        const Operation& o = mrl.ops[pc];
        const char* name;
        if (o.op < LL_NUM_OF_OPS)
            name = Vm::Code::op_names[o.op];
        else
            name = opName(o.op);

        out << "    " << QString("%1: ").arg(pc, 4);

        switch (o.op)
        {
        case LL_add_i4: case LL_add_i8: case LL_add_r4: case LL_add_r8:
        case LL_sub_i4: case LL_sub_i8: case LL_sub_r4: case LL_sub_r8:
        case LL_mul_i4: case LL_mul_i8: case LL_mul_r4: case LL_mul_r8:
        case LL_div_i4: case LL_div_i8: case LL_div_r4: case LL_div_r8:
        case LL_div_un_i4: case LL_div_un_i8:
        case LL_rem_i4: case LL_rem_i8: case LL_rem_un_i4: case LL_rem_un_i8:
        case LL_and_i4: case LL_and_i8: case LL_or_i4: case LL_or_i8:
        case LL_xor_i4: case LL_xor_i8:
        case LL_shl_i4: case LL_shl_i8: case LL_shr_i4: case LL_shr_i8:
        case LL_shr_un_i4: case LL_shr_un_i8:
        case LL_ceq_i4: case LL_ceq_i8: case LL_ceq_r4: case LL_ceq_r8:
        case LL_ceq_p: case LL_ceq_pp:
        case LL_cgt_i4: case LL_cgt_i8: case LL_cgt_r4: case LL_cgt_r8:
        case LL_cgt_u4: case LL_cgt_u8: case LL_cgt_p:
        case LL_clt_i4: case LL_clt_i8: case LL_clt_r4: case LL_clt_r8:
        case LL_clt_u4: case LL_clt_u8: case LL_clt_p:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            out << ", ";
            printReg(out, o.r3);
            break;

        case LL_abs_i4: case LL_abs_i8: case LL_abs_r4: case LL_abs_r8:
        case LL_neg_i4: case LL_neg_i8: case LL_neg_r4: case LL_neg_r8:
        case LL_not_i4: case LL_not_i8:
        case LL_conv_i1_i4: case LL_conv_i1_i8: case LL_conv_i1_r4: case LL_conv_i1_r8:
        case LL_conv_i2_i4: case LL_conv_i2_i8: case LL_conv_i2_r4: case LL_conv_i2_r8:
        case LL_conv_i4_i8: case LL_conv_i4_r4: case LL_conv_i4_r8:
        case LL_conv_i8_i4: case LL_conv_i8_r4: case LL_conv_i8_r8:
        case LL_conv_u1_i4: case LL_conv_u1_i8: case LL_conv_u1_r4: case LL_conv_u1_r8:
        case LL_conv_u2_i4: case LL_conv_u2_i8: case LL_conv_u2_r4: case LL_conv_u2_r8:
        case LL_conv_u4_i8: case LL_conv_u4_r4: case LL_conv_u4_r8:
        case LL_conv_u8_i4: case LL_conv_u8_r4: case LL_conv_u8_r8:
        case LL_conv_r4_i4: case LL_conv_r4_i8: case LL_conv_r4_r8:
        case LL_conv_r8_i4: case LL_conv_r8_i8: case LL_conv_r8_r4:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            break;

        case LL_ldloc_i1: case LL_ldloc_i2: case LL_ldloc_i4: case LL_ldloc_i8:
        case LL_ldloc_u1: case LL_ldloc_u2: case LL_ldloc_u4: case LL_ldloc_u8:
        case LL_ldloc_r4: case LL_ldloc_r8: case LL_ldloc_p: case LL_ldloc_pp:
        case LL_ldloca:
        case LL_ldarg_i1: case LL_ldarg_i2: case LL_ldarg_i4: case LL_ldarg_i8:
        case LL_ldarg_u1: case LL_ldarg_u2: case LL_ldarg_u4: case LL_ldarg_u8:
        case LL_ldarg_r4: case LL_ldarg_r8: case LL_ldarg_p: case LL_ldarg_pp:
        case LL_ldarga:
        case LL_ldvar_i1: case LL_ldvar_i2: case LL_ldvar_i4: case LL_ldvar_i8:
        case LL_ldvar_u1: case LL_ldvar_u2: case LL_ldvar_u4: case LL_ldvar_u8:
        case LL_ldvar_r4: case LL_ldvar_r8: case LL_ldvar_p: case LL_ldvar_pp:
        case LL_ldvara:
            printReg(out, o.r1);
            out << " = " << name << " " << o.aux;
            break;

        case LL_ldloc_vt: case LL_ldarg_vt: case LL_ldvar_vt:
            printReg(out, o.r1);
            out << " = " << name << " " << o.aux << " (" << o.aux2 << ")";
            break;

        case LL_stloc_i1: case LL_stloc_i2: case LL_stloc_i4: case LL_stloc_i8:
        case LL_stloc_r4: case LL_stloc_r8: case LL_stloc_p: case LL_stloc_pp:
        case LL_starg_i1: case LL_starg_i2: case LL_starg_i4: case LL_starg_i8:
        case LL_starg_r4: case LL_starg_r8: case LL_starg_p: case LL_starg_pp:
        case LL_stvar_i1: case LL_stvar_i2: case LL_stvar_i4: case LL_stvar_i8:
        case LL_stvar_r4: case LL_stvar_r8: case LL_stvar_p: case LL_stvar_pp:
            out << name << " ";
            printReg(out, o.r1);
            out << ", " << o.aux;
            break;

        case LL_stloc_vt: case LL_starg_vt: case LL_stvar_vt:
            out << name << " ";
            printReg(out, o.r1);
            out << ", " << o.aux << " (" << o.aux2 << ")";
            break;

        case LL_ldfld_i1: case LL_ldfld_i2: case LL_ldfld_i4: case LL_ldfld_i8:
        case LL_ldfld_u1: case LL_ldfld_u2: case LL_ldfld_u4: case LL_ldfld_u8:
        case LL_ldfld_r4: case LL_ldfld_r8: case LL_ldfld_p: case LL_ldfld_pp:
        case LL_ldflda:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            out << ", " << o.aux;
            break;

        case LL_ldfld_vt:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            out << ", " << o.aux << " (" << o.aux2 << ")";
            break;

        case LL_stfld_i1: case LL_stfld_i2: case LL_stfld_i4: case LL_stfld_i8:
        case LL_stfld_r4: case LL_stfld_r8: case LL_stfld_p: case LL_stfld_pp:
            out << name << " ";
            printReg(out, o.r1); // obj
            out << ", ";
            printReg(out, o.r2); // value
            out << ", " << o.aux;
            break;

        case LL_stfld_vt:
            out << name << " ";
            printReg(out, o.r1);
            out << ", ";
            printReg(out, o.r2);
            out << ", " << o.aux << " (" << o.aux2 << ")";
            break;

        case LL_ldelem_i1: case LL_ldelem_i2: case LL_ldelem_i4: case LL_ldelem_i8:
        case LL_ldelem_u1: case LL_ldelem_u2: case LL_ldelem_u4: case LL_ldelem_u8:
        case LL_ldelem_r4: case LL_ldelem_r8: case LL_ldelem_p:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            out << ", ";
            printReg(out, o.r3);
            break;

        case LL_ldelem_vt:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            out << ", ";
            printReg(out, o.r3);
            out << " (" << o.aux << ")";
            break;

        case LL_ldelema:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            out << ", ";
            printReg(out, o.r3);
            out << " [" << o.aux << "]";
            break;

        case LL_stelem_i1: case LL_stelem_i2: case LL_stelem_i4: case LL_stelem_i8:
        case LL_stelem_r4: case LL_stelem_r8: case LL_stelem_p:
            out << name << " ";
            printReg(out, o.r1); // arr
            out << ", ";
            printReg(out, o.r2); // idx
            out << ", ";
            printReg(out, o.r3); // val
            break;

        case LL_stelem_vt:
            out << name << " ";
            printReg(out, o.r1);
            out << ", ";
            printReg(out, o.r2);
            out << ", ";
            printReg(out, o.r3);
            out << " (" << o.aux << ")";
            break;

        case LL_ldind_i1: case LL_ldind_i2: case LL_ldind_i4: case LL_ldind_i8:
        case LL_ldind_u1: case LL_ldind_u2: case LL_ldind_u4: case LL_ldind_u8:
        case LL_ldind_r4: case LL_ldind_r8: case LL_ldind_p:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            break;

        case LL_ldind_vt: case LL_ldind_str:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            out << " (" << o.aux << ")";
            break;

        case LL_stind_i1: case LL_stind_i2: case LL_stind_i4: case LL_stind_i8:
        case LL_stind_r4: case LL_stind_r8: case LL_stind_p:
            out << name << " ";
            printReg(out, o.r1); // ptr
            out << ", ";
            printReg(out, o.r2); // val
            break;

        case LL_stind_vt:
            out << name << " ";
            printReg(out, o.r1);
            out << ", ";
            printReg(out, o.r2);
            out << " (" << o.aux << ")";
            break;

        case LL_ldc_i4:
            printReg(out, o.r1);
            out << " = " << name << " " << d_code.getInt(o.aux);
            break;
        case LL_ldc_i8:
            printReg(out, o.r1);
            out << " = " << name << " " << d_code.getInt(o.aux);
            break;
        case LL_ldc_r4: case LL_ldc_r8:
            printReg(out, o.r1);
            out << " = " << name << " " << d_code.getDouble(o.aux);
            break;
        case LL_ldc_i4_m1:
            printReg(out, o.r1);
            out << " = ldc_i4 -1";
            break;
        case LL_ldc_i4_0: case LL_ldc_i4_1: case LL_ldc_i4_2: case LL_ldc_i4_3:
        case LL_ldc_i4_4: case LL_ldc_i4_5: case LL_ldc_i4_6: case LL_ldc_i4_7:
        case LL_ldc_i4_8:
            printReg(out, o.r1);
            out << " = ldc_i4 " << (o.op - LL_ldc_i4_0);
            break;
        case LL_ldnull:
            printReg(out, o.r1);
            out << " = ldnull";
            break;
        case LL_ldstr:
            printReg(out, o.r1);
            out << " = ldstr \"" << d_code.getString(o.aux) << "\"";
            break;
        case LL_ldobj:
            printReg(out, o.r1);
            out << " = ldobj #" << o.aux;
            break;

        case LL_br:
            out << "br L" << o.aux;
            break;
        case LL_brfalse_i4:
            out << "brfalse_i4 ";
            printReg(out, o.r1);
            out << ", L" << o.aux;
            break;

        case LL_ldproc:
            printReg(out, o.r1);
            if (o.aux < (quint32)d_code.procCount())
                out << " = ldproc " << d_code.getProc(o.aux)->decl->toPath();
            else
                out << " = ldproc #" << o.aux;
            break;

        case LL_ldmeth:
            printReg(out, o.r1);
            out << " = ldmeth ";
            printReg(out, o.r2);
            out << ", slot " << o.aux;
            break;

        case LL_ldmeth_struct:
            printReg(out, o.r1);
            out << " = ldmeth_struct ";
            printReg(out, o.r2);
            if (o.aux < (quint32)d_code.procCount())
                out << ", " << d_code.getProc(o.aux)->decl->toPath();
            else
                out << ", #" << o.aux;
            break;

        case LL_ldmeth_iface:
            printReg(out, o.r1);
            out << " = ldmeth_iface ";
            printReg(out, o.r2);
            out << ", slot " << o.aux;
            break;

        case LL_ldiface:
            printReg(out, o.r1);
            out << " = ldiface ";
            printReg(out, o.r2);
            out << ", vtable " << o.aux;
            break;

        case LL_call: case LL_callvirt: case LL_callinst:
            if (o.r1 != NO_REG) {
                printReg(out, o.r1);
                out << " = ";
            }
            out << name;
            if (o.aux < (quint32)d_code.procCount())
                out << " " << d_code.getProc(o.aux)->decl->toPath();
            else
                out << " #" << o.aux;
            break;

        case LL_calli:
            if (o.r1 != NO_REG) {
                printReg(out, o.r1);
                out << " = ";
            }
            out << "calli ";
            printReg(out, o.r2);
            break;

        case LL_callmi:
            if (o.r1 != NO_REG) {
                printReg(out, o.r1);
                out << " = ";
            }
            out << "callmi ";
            printReg(out, o.r2);
            break;

        case LL_ret:
            out << "ret ";
            printReg(out, o.r1);
            break;
        case LL_ret_void:
            out << "ret_void";
            break;

        case RL_arg:
            out << "arg ";
            printReg(out, o.r1);
            out << ", " << o.aux;
            break;

        case RL_mov:
            printReg(out, o.r1);
            out << " = mov ";
            printReg(out, o.r2);
            break;

        case LL_alloc1: case LL_calloc1: case LL_gcalloc1:
            printReg(out, o.r1);
            out << " = " << name;
            if (o.templ)
                out << " template";
            out << " " << o.aux;
            break;

        case LL_allocN: case LL_callocN: case LL_gcallocN:
            printReg(out, o.r1);
            out << " = " << name << " ";
            printReg(out, o.r2);
            if (o.templ)
                out << " template";
            out << " " << o.aux;
            break;

        case LL_free:
            out << "free ";
            printReg(out, o.r1);
            break;

        case LL_initobj:
            out << "initobj ";
            printReg(out, o.r1);
            if (o.templ)
                out << " template";
            out << " " << o.aux;
            break;

        case LL_strcpy:
            out << "strcpy ";
            printReg(out, o.r1); // lhs
            out << ", ";
            printReg(out, o.r2); // rhs
            out << ", " << o.aux;
            break;

        case LL_isinst:
            printReg(out, o.r1);
            out << " = isinst ";
            printReg(out, o.r2);
            if (o.aux < (quint32)d_code.vtableCount())
                out << ", " << d_code.getVtable(o.aux)->type->decl->toPath();
            else
                out << ", vtable " << o.aux;
            break;

        case LL_line:
            out << "line " << o.aux;
            break;
        case LL_nop:
            out << "nop";
            break;
        case LL_cli:
            out << "cli";
            break;
        case LL_sti:
            out << "sti";
            break;
        case LL_putreg:
            out << "putreg ";
            printReg(out, o.r1);
            out << ", " << (o.aux & 0xFFFF) << ":" << (o.aux >> 16 & 0xFF);
            break;
        case LL_getreg:
            printReg(out, o.r1);
            out << " = getreg " << (o.aux & 0xFFFF) << ":" << (o.aux >> 16 & 0xFF);
            break;

        default:
            out << name << " ???";
            break;
        }

        out << endl;
    }

    return true;
}

bool Rl::Code::dumpModule(QTextStream& out, Declaration* module)
{
    for (Declaration* d = module->subs; d; d = d->next)
    {
        if (d->kind == Declaration::Procedure)
        {
            int i = d_code.findProc(d);
            if (i >= 0)
                dumpProc(out, i);
        }
        else if (d->kind == Declaration::TypeDecl && d->getType())
        {
            Type* t = d->getType();
            if (t->kind == Type::NameRef)
                t = t->getType();
            if (t && (t->kind == Type::Object || t->kind == Type::Struct))
            {
                for (int j = 0; j < t->subs.size(); j++)
                {
                    Declaration* sub = t->subs[j];
                    if (sub->kind == Declaration::Procedure)
                    {
                        int i = d_code.findProc(sub);
                        if (i >= 0)
                            dumpProc(out, i);
                    }
                }
            }
        }
    }
    return true;
}

bool Rl::Code::dumpAll(QTextStream& out)
{
    for (int i = 0; i < (int)d_procs.size(); i++)
    {
        if (!d_procs[i].llProc->external && !d_procs[i].ops.empty())
            dumpProc(out, i);
    }
    return true;
}
