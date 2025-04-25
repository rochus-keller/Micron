/*
* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
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

OPDEF(add_i4, NoOpArgs), OPDEF(add_i8, NoOpArgs), OPDEF(add_r4, NoOpArgs), OPDEF(add_r8, NoOpArgs),
OPDEF(sub_i4, NoOpArgs), OPDEF(sub_i8, NoOpArgs), OPDEF(sub_r4, NoOpArgs), OPDEF(sub_r8, NoOpArgs),
OPDEF(mul_i4, NoOpArgs), OPDEF(mul_i8, NoOpArgs), OPDEF(mul_r4, NoOpArgs), OPDEF(mul_r8, NoOpArgs),
OPDEF(div_i4, NoOpArgs), OPDEF(div_i8, NoOpArgs), OPDEF(div_r4, NoOpArgs), OPDEF(div_r8, NoOpArgs),
OPDEF(div_un_i4, NoOpArgs), OPDEF(div_un_i8, NoOpArgs),
OPDEF(rem_i4, NoOpArgs), OPDEF(rem_i8, NoOpArgs), OPDEF(rem_r4, NoOpArgs), OPDEF(rem_r8, NoOpArgs),
OPDEF(rem_un_i4, NoOpArgs), OPDEF(rem_un_i8, NoOpArgs),
OPDEF(abs_i4, NoOpArgs), OPDEF(abs_i8, NoOpArgs), OPDEF(abs_r4, NoOpArgs), OPDEF(abs_r8, NoOpArgs),
OPDEF(neg_i4, NoOpArgs), OPDEF(neg_i8, NoOpArgs), OPDEF(neg_r4, NoOpArgs), OPDEF(neg_r8, NoOpArgs),

OPDEF(and_i4, NoOpArgs), OPDEF(and_i8, NoOpArgs), OPDEF(or_i4, NoOpArgs), OPDEF(or_i8, NoOpArgs),
OPDEF(xor_i4, NoOpArgs), OPDEF(xor_i8, NoOpArgs), OPDEF(not_i4, NoOpArgs), OPDEF(not_i8, NoOpArgs),
OPDEF(shl_i4, NoOpArgs), OPDEF(shl_i8, NoOpArgs), OPDEF(shr_i4, NoOpArgs), OPDEF(shr_i8, NoOpArgs),
OPDEF(shr_un_i4, NoOpArgs), OPDEF(shr_un_i8, NoOpArgs),

OPDEF(ceq_i4, NoOpArgs), OPDEF(ceq_i8, NoOpArgs), OPDEF(ceq_r4, NoOpArgs), OPDEF(ceq_r8, NoOpArgs),
OPDEF(cgt_i4, NoOpArgs), OPDEF(cgt_i8, NoOpArgs), OPDEF(cgt_r4, NoOpArgs), OPDEF(cgt_r8, NoOpArgs),
OPDEF(cgt_u4, NoOpArgs), OPDEF(cgt_u8, NoOpArgs),
OPDEF(clt_i4, NoOpArgs), OPDEF(clt_i8, NoOpArgs), OPDEF(clt_r4, NoOpArgs), OPDEF(clt_r8, NoOpArgs),
OPDEF(clt_u4, NoOpArgs), OPDEF(clt_u8, NoOpArgs),

OPDEF(conv_i1_i4, NoOpArgs), OPDEF(conv_i1_i8, NoOpArgs), OPDEF(conv_i1_r4, NoOpArgs), OPDEF(conv_i1_r8, NoOpArgs), // to_from
OPDEF(conv_i2_i4, NoOpArgs), OPDEF(conv_i2_i8, NoOpArgs), OPDEF(conv_i2_r4, NoOpArgs), OPDEF(conv_i2_r8, NoOpArgs),
OPDEF(conv_i4_i8, NoOpArgs), OPDEF(conv_i4_r4, NoOpArgs), OPDEF(conv_i4_r8, NoOpArgs),
OPDEF(conv_i8_i4, NoOpArgs), OPDEF(conv_i8_r4, NoOpArgs), OPDEF(conv_i8_r8, NoOpArgs),

OPDEF(conv_u1_i4, NoOpArgs), OPDEF(conv_u1_i8, NoOpArgs), OPDEF(conv_u1_r4, NoOpArgs), OPDEF(conv_u1_r8, NoOpArgs),
OPDEF(conv_u2_i4, NoOpArgs), OPDEF(conv_u2_i8, NoOpArgs), OPDEF(conv_u2_r4, NoOpArgs), OPDEF(conv_u2_r8, NoOpArgs),
OPDEF(conv_u4_i8, NoOpArgs), OPDEF(conv_u4_r4, NoOpArgs), OPDEF(conv_u4_r8, NoOpArgs),
OPDEF(conv_u8_i4, NoOpArgs), OPDEF(conv_u8_r4, NoOpArgs), OPDEF(conv_u8_r8, NoOpArgs),

OPDEF(conv_r4_i4, NoOpArgs), OPDEF(conv_r4_i8, NoOpArgs), OPDEF(conv_r4_r8, NoOpArgs),
OPDEF(conv_r8_i4, NoOpArgs), OPDEF(conv_r8_i8, NoOpArgs), OPDEF(conv_r8_r4, NoOpArgs),

OPDEF(ldarg_i1, OffArg), OPDEF(ldarg_i2, OffArg), OPDEF(ldarg_i4, OffArg), OPDEF(ldarg_i8, OffArg),
OPDEF(ldarg_u1, OffArg), OPDEF(ldarg_u2, OffArg), OPDEF(ldarg_u4, OffArg), OPDEF(ldarg_u8, OffArg),
OPDEF(ldarg_r4, OffArg), OPDEF(ldarg_r8, OffArg), OPDEF(ldarg_p, OffArg), OPDEF(ldarg_pp, OffArg),
OPDEF(ldarg_vt, OffSizeArgs), OPDEF(ldarga, OffArg),

OPDEF(starg_i1, OffArg), OPDEF(starg_i2, OffArg), OPDEF(starg_i4, OffArg), OPDEF(starg_i8, OffArg),
OPDEF(starg_r4, OffArg), OPDEF(starg_r8, OffArg), OPDEF(starg_p, OffArg), OPDEF(starg_pp, OffArg),
OPDEF(starg_vt, OffSizeArgs),

OPDEF(ldelem_i1, NoOpArgs), OPDEF(ldelem_i2, NoOpArgs), OPDEF(ldelem_i4, NoOpArgs), OPDEF(ldelem_i8, NoOpArgs),
OPDEF(ldelem_u1, NoOpArgs), OPDEF(ldelem_u2, NoOpArgs), OPDEF(ldelem_u4, NoOpArgs), OPDEF(ldelem_u8, NoOpArgs),
OPDEF(ldelem_r4, NoOpArgs), OPDEF(ldelem_r8, NoOpArgs), OPDEF(ldelem_p, NoOpArgs), OPDEF(ldelem_pp, NoOpArgs),
OPDEF(ldelema, NoOpArgs), OPDEF(ldelem_vt, SizeArg),

OPDEF(stelem_i1, NoOpArgs), OPDEF(stelem_i2, NoOpArgs), OPDEF(stelem_i4, NoOpArgs), OPDEF(stelem_i8, NoOpArgs),
OPDEF(stelem_r4, NoOpArgs), OPDEF(stelem_r8, NoOpArgs), OPDEF(stelem_p, NoOpArgs), OPDEF(stelem_pp, NoOpArgs),
OPDEF(stelem_vt, SizeArg),

OPDEF(ldfld_i1, OffArg), OPDEF(ldfld_i2, OffArg), OPDEF(ldfld_i4, OffArg), OPDEF(ldfld_i8, OffArg),
OPDEF(ldfld_u1, OffArg), OPDEF(ldfld_u2, OffArg), OPDEF(ldfld_u4, OffArg), OPDEF(ldfld_u8, OffArg),
OPDEF(ldfld_r4, OffArg), OPDEF(ldfld_r8, OffArg), OPDEF(ldfld_p, OffArg), OPDEF(ldfld_pp, OffArg),
OPDEF(ldflda, OffArg), OPDEF(ldfld_vt, OffSizeArgs),

OPDEF(stfld_i1, OffArg), OPDEF(stfld_i2, OffArg), OPDEF(stfld_i4, OffArg), OPDEF(stfld_i8, OffArg),
OPDEF(stfld_u1, OffArg), OPDEF(stfld_u2, OffArg), OPDEF(stfld_u4, OffArg), OPDEF(stfld_u8, OffArg),
OPDEF(stfld_r4, OffArg), OPDEF(stfld_r8, OffArg), OPDEF(stfld_p, OffArg), OPDEF(stfld_pp, OffArg),
OPDEF(stfld_vt, OffSizeArgs),

OPDEF(ldind_i1, NoOpArgs), OPDEF(ldind_i2, NoOpArgs), OPDEF(ldind_i4, NoOpArgs), OPDEF(ldind_i8, NoOpArgs),
OPDEF(ldind_u1, NoOpArgs), OPDEF(ldind_u2, NoOpArgs), OPDEF(ldind_u4, NoOpArgs), OPDEF(ldind_u8, NoOpArgs),
OPDEF(ldind_r4, NoOpArgs), OPDEF(ldind_r8, NoOpArgs), OPDEF(ldind_p, NoOpArgs), OPDEF(ldind_pp, NoOpArgs),
OPDEF(ldind_vt, SizeArg),

OPDEF(stind_i1, NoOpArgs), OPDEF(stind_i2, NoOpArgs), OPDEF(stind_i4, NoOpArgs), OPDEF(stind_i8, NoOpArgs),
OPDEF(stind_r4, NoOpArgs), OPDEF(stind_r8, NoOpArgs), OPDEF(stind_p, NoOpArgs), OPDEF(stind_pp, NoOpArgs),
OPDEF(stind_vt, SizeArg),

OPDEF(ldloc_i1, OffArg), OPDEF(ldloc_i2, OffArg), OPDEF(ldloc_i4, OffArg), OPDEF(ldloc_i8, OffArg),
OPDEF(ldloc_u1, OffArg), OPDEF(ldloc_u2, OffArg), OPDEF(ldloc_u4, OffArg), OPDEF(ldloc_u8, OffArg),
OPDEF(ldloc_r4, OffArg), OPDEF(ldloc_r8, OffArg), OPDEF(ldloc_p, OffArg), OPDEF(ldloc_pp, OffArg),
OPDEF(ldloca, OffArg), OPDEF(ldloc_vt, OffSizeArgs),

OPDEF(stloc_i1, OffArg), OPDEF(stloc_i2, OffArg), OPDEF(stloc_i4, OffArg), OPDEF(stloc_i8, OffArg),
OPDEF(stloc_r4, OffArg), OPDEF(stloc_r8, OffArg), OPDEF(stloc_p, OffArg), OPDEF(stloc_pp, OffArg),
OPDEF(stloc_vt, OffSizeArgs),

OPDEF(ldvar_i1, OffArg), OPDEF(ldvar_i2, OffArg), OPDEF(ldvar_i4, OffArg), OPDEF(ldvar_i8, OffArg),
OPDEF(ldvar_u1, OffArg), OPDEF(ldvar_u2, OffArg), OPDEF(ldvar_u4, OffArg), OPDEF(ldvar_u8, OffArg),
OPDEF(ldvar_r4, OffArg), OPDEF(ldvar_r8, OffArg), OPDEF(ldvar_p, OffArg), OPDEF(ldvar_pp, OffArg),
OPDEF(ldvara, OffArg), OPDEF(ldvar_vt, OffSizeArgs),

OPDEF(stvar_i1, OffArg), OPDEF(stvar_i2, OffArg), OPDEF(stvar_i4, OffArg), OPDEF(stvar_i8, OffArg),
OPDEF(stvar_u1, OffArg), OPDEF(stvar_u2, OffArg), OPDEF(stvar_u4, OffArg), OPDEF(stvar_u8, OffArg),
OPDEF(stvar_r4, OffArg), OPDEF(stvar_r8, OffArg), OPDEF(stvar_p, OffArg), OPDEF(stvar_pp, OffArg),
OPDEF(stvar_vt, OffSizeArgs),

OPDEF(ldc_i4, IntArg), OPDEF(ldc_i8, IntArg), OPDEF(ldc_r4, FloatArg), OPDEF(ldc_r8, FloatArg),
OPDEF(ldc_i4_m1, NoOpArgs), OPDEF(ldc_i4_0, NoOpArgs), OPDEF(ldc_i4_1, NoOpArgs),
OPDEF(ldc_i4_2, NoOpArgs), OPDEF(ldc_i4_3, NoOpArgs),
OPDEF(ldc_i4_4, NoOpArgs), OPDEF(ldc_i4_5, NoOpArgs),OPDEF(ldc_i4_6, NoOpArgs),
OPDEF(ldc_i4_7, NoOpArgs), OPDEF(ldc_i4_8, NoOpArgs),

OPDEF(ldnull, NoOpArgs), OPDEF(ldstr, StrArg),
OPDEF(ldobj, 1), // TODO

OPDEF(br, JumpArg),
OPDEF(brtrue_i4, JumpArg), OPDEF(brtrue_i8, JumpArg),
OPDEF(brfalse_i4, JumpArg), OPDEF(brfalse_i8, JumpArg),

OPDEF(ldproc, ProcArg), OPDEF(ldmeth, ProcArg),
OPDEF(sizeof, 1), // TODO
OPDEF(ptroff, NoOpArgs),
OPDEF(nop, NoOpArgs), OPDEF(pop, SizeArg), OPDEF(dup, SizeArg),
OPDEF(ret, SizeArg), OPDEF(ret_void, NoOpArgs),
OPDEF(call, ProcArg), OPDEF(calli, NoOpArgs),
OPDEF(alloc1, SizeArg), OPDEF(allocN, SizeArg), OPDEF(free, NoOpArgs),

OPDEF(vt_size, SizeArg), // suffix of ldarg_vt, starg_vt, ldloc_vt, stloc_vt, ldfld_vt, stfld_vt,
                //   ldvar_vt, stvar_vt

// TODO:
OPDEF(newvla, NoOpArgs),
OPDEF(callvi, NoOpArgs), OPDEF(callvirt, NoOpArgs),
OPDEF(castptr, NoOpArgs), OPDEF(conv_ip, NoOpArgs),
OPDEF(line, NoOpArgs),
OPDEF(initobj, NoOpArgs), OPDEF(isinst, NoOpArgs),

// special implementation ops
OPDEF(already_called, NoOpArgs), // puts int32 1 or 0 on stack

