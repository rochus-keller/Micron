/*
* Copyright 2019-2024 Rochus Keller <mailto:me@rochus-keller.ch>
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
#include "MicMilOp.h"

namespace Mic
{
const char* s_opName[] =
{
    "invalid",
    // expressions
    "add", "and", "call", "calli", "castptr",
    "ceq", "cgt", "cgt_un", "clt", "clt_un",
    "conv_i1", "conv_i2", "conv_i4", "conv_i8", "conv_r4", "conv_r8",
    "conv_u1", "conv_u2", "conv_u4", "conv_u8", "conv_ip",
    "div", "div_un", "dup", "initobj", "ldarg", "ldarg_s",
    "ldarg_0", "ldarg_1", "ldarg_2", "ldarg_3",
    "ldarga", "ldarga_s",
    "ldc_i4", "ldc_i8", "ldc_i4_s", "ldc_r4", "ldc_r8",
    "ldc_i4_0", "ldc_i4_1", "ldc_i4_2", "ldc_i4_3", "ldc_i4_4", "ldc_i4_5",
    "ldc_i4_6", "ldc_i4_7", "ldc_i4_8", "ldc_i4_m1", "ldc_obj",
    "ldelem", "ldelema", "ldelem_i1", "ldelem_i2",
    "ldelem_i4", "ldelem_i8", "ldelem_u1", "ldelem_u2",
    "ldelem_u4", "ldelem_u8", "ldelem_r4", "ldelem_r8", "ldelem_ip",
    "ldfld", "ldflda",
    "ldind_i1", "ldind_i2", "ldind_i4", "ldind_i8", "ldind_u1", "ldind_u2",
    "ldind_u4", "ldind_r4", "ldind_u8", "ldind_r8", "ldind_ip",
    "ldloc", "ldloc_s", "ldloca", "ldloca_s",
    "ldloc_0", "ldloc_1", "ldloc_2", "ldloc_3", "ldnull",
    "ldobj", "ldproc", "ldstr",
    "ldvar", "ldvara", "mul", "neg",
    "newarr", "newvla", "newobj",
    "not", "or", "rem", "rem_un", "shl", "shr", "shr_un", "sizeof", "sub", "xor",
    // statements
    "disp", "repeat", "until",
    "exit", "goto", "if", "then", "else", "end",
    "label", "line", "loop", "pop", "ret",
    "starg", "starg_s",
    "stelem", "stelem_i1", "stelem_i2", "stelem_i4", "stelem_i8",
    "stelem_r4", "stelem_r8", "stelem_ip", "stfld",
    "stind_i1", "stind_i2", "stind_i4", "stind_i8", "stind_r4", "stind_r8", "stind_ip",
    "stloc", "stloc_s", "stloc_0", "stloc_1", "stloc_2", "stloc_3",
    "stobj", "stvar", "switch", "case", "while", "do"
};
}
