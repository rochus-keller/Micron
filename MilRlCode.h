#ifndef MILRLCODE_H
#define MILRLCODE_H

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

#include <Micron/MilVmCode.h>
#include <vector>

class QTextStream;

namespace Mil
{
namespace Rl
{

enum { NO_REG = 0xFFFF };

enum ExtraOp {
    RL_mov = Vm::LL_NUM_OF_OPS, // register copy: r1 = r2
    RL_arg,             // call argument: r1 at byte offset aux
    RL_NUM_OF_OPS
};

struct Operation
{
    quint16 op : 15;     // LL or RL code
    quint16 templ : 1;   // template flage
    quint16 r1, r2, r3;     // register operands
    // MRL is only a temporary representation, so we can afford to waste a few bytes here:
    quint32 aux;    // offset, const index, proc index, branch target
    quint32 aux2;   // vt size for _vt ops, encoded callinfo for calli/callmi

    Operation(): op(0), r1(NO_REG), r2(NO_REG), r3(NO_REG),
             aux(0), aux2(0),templ(false) {}
    Operation(quint16 op, quint16 r1, quint16 r2 = NO_REG, quint16 r3 = NO_REG,
          quint32 aux = 0, quint32 aux2 = 0, bool templ = false)
        : op(op), r1(r1), r2(r2), r3(r3), aux(aux), aux2(aux2),templ(templ) {}
};

struct Procedure
{
    std::vector<Operation> ops;
    Vm::Procedure* llProc;
    quint16 numVregs;   // total virtual registers allocated

    Procedure(): llProc(0), numVregs(0) {}
};

class Code
{
public:
    Code(Vm::Code& llCode, quint8 stackAlignment = 8);

    bool compile();
    bool compileProc(int procIdx);
    bool compactAll(int maxRegs = 250);
    bool compactRegisters(int procIdx, int maxRegs = 250);

    bool dumpAll(QTextStream& out);
    bool dumpProc(QTextStream& out, int procIdx);
    bool dumpModule(QTextStream& out, Declaration* module);

    Procedure* getProc(int i) { return &d_procs[i]; }
    int procCount() const { return d_procs.size(); }

    struct Stats {
        int procsLowered;
        int totalVregs;
        int totalMrlOps;
        int totalLlOps;
        Stats(): procsLowered(0), totalVregs(0), totalMrlOps(0), totalLlOps(0) {}
    };
    const Stats& getStats() const { return d_stats; }

private:
    Vm::Code& d_code;
    std::vector<Procedure> d_procs;
    quint8 d_stackAlign;
    Stats d_stats;

    static const char* opName(int op);
};

} // Mrl
} // Mil

#endif // MILRLCODE_H
