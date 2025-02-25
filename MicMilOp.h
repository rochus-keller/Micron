#ifndef MICMILOP_H
#define MICMILOP_H

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

namespace Mic
{

enum IL_op
{
    IL_invalid,
    // expressions
    IL_add, IL_and, IL_call, IL_calli, IL_callvirt, IL_castptr,
    IL_ceq, IL_cgt, IL_cgt_un, IL_clt, IL_clt_un,
    IL_conv_i1, IL_conv_i2, IL_conv_i4, IL_conv_i8, IL_conv_r4, IL_conv_r8,
    IL_conv_u1, IL_conv_u2, IL_conv_u4, IL_conv_u8, IL_conv_ip,
    IL_div, IL_div_un, IL_dup, IL_iif, IL_initobj, IL_ldarg, IL_ldarg_s,
    IL_ldarg_0, IL_ldarg_1, IL_ldarg_2, IL_ldarg_3,
    IL_ldarga, IL_ldarga_s,
    IL_ldc_i4, IL_ldc_i8, IL_ldc_i4_s, IL_ldc_r4, IL_ldc_r8,
    IL_ldc_i4_0, IL_ldc_i4_1, IL_ldc_i4_2, IL_ldc_i4_3, IL_ldc_i4_4, IL_ldc_i4_5,
    IL_ldc_i4_6, IL_ldc_i4_7, IL_ldc_i4_8, IL_ldc_i4_m1, IL_ldc_obj,
    IL_ldelem, IL_ldelema, IL_ldelem_i1, IL_ldelem_i2,
    IL_ldelem_i4, IL_ldelem_i8, IL_ldelem_u1, IL_ldelem_u2,
    IL_ldelem_u4, IL_ldelem_u8, IL_ldelem_r4, IL_ldelem_r8, IL_ldelem_ip,
    IL_ldfld, IL_ldflda,
    IL_ldind_i1, IL_ldind_i2, IL_ldind_i4, IL_ldind_i8, IL_ldind_u1, IL_ldind_u2,
    IL_ldind_u4, IL_ldind_r4, IL_ldind_u8, IL_ldind_r8, IL_ldind_ip,
    IL_ldloc, IL_ldloc_s, IL_ldloca, IL_ldloca_s,
    IL_ldloc_0, IL_ldloc_1, IL_ldloc_2, IL_ldloc_3, IL_ldnull,
    IL_ldobj, IL_ldproc, IL_ldmeth, IL_ldstr,
    IL_ldvar, IL_ldvara, IL_mul, IL_neg,
    IL_newarr, IL_newvla, IL_newobj,
    IL_not, IL_or, IL_rem, IL_rem_un, IL_shl, IL_shr, IL_shr_un,
    IL_sizeof, IL_sub, IL_xor, IL_ptroff, IL_nop,
    // statements
    IL_free, IL_repeat, IL_until,
    IL_exit, IL_goto, IL_ifgoto, IL_if, IL_then, IL_else, IL_end,
    IL_label, IL_line, IL_loop, IL_pop, IL_ret,
    IL_starg, IL_starg_s,
    IL_stelem, IL_stelem_i1, IL_stelem_i2, IL_stelem_i4, IL_stelem_i8,
    IL_stelem_r4, IL_stelem_r8, IL_stelem_ip, IL_stfld,
    IL_stind_i1, IL_stind_i2, IL_stind_i4, IL_stind_i8, IL_stind_r4, IL_stind_r8, IL_stind_ip,
    IL_stloc, IL_stloc_s, IL_stloc_0, IL_stloc_1, IL_stloc_2, IL_stloc_3,
    IL_stobj, IL_stvar, IL_switch, IL_case, IL_while, IL_do,
    IL_NUM_OF_OPS
}; // Update s_opName !!!

extern const char* s_opName[];

}

#endif // MICMILOP_H
