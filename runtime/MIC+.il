.assembly extern mscorlib {}
.assembly 'MIC$'
{
  .custom instance void class [mscorlib]System.Runtime.CompilerServices.RuntimeCompatibilityAttribute::'.ctor'() =  (
		01 00 01 00 54 02 16 57 72 61 70 4E 6F 6E 45 78   // ....T..WrapNonEx
		63 65 70 74 69 6F 6E 54 68 72 6F 77 73 01       ) // ceptionThrows.

  .permissionset reqmin = {[mscorlib]System.Security.Permissions.SecurityPermissionAttribute = {property bool 'SkipVerification' = bool(true)}}
  .hash algorithm 0x00008004
  .ver  0:0:0:0
}
.module 'MIC+.dll' // GUID = {AAA9E176-294A-46E0-9427-DB4CBF75F71C}

.custom instance void class [mscorlib]System.Security.UnverifiableCodeAttribute::'.ctor'() =  (01 00 00 00 ) // ....


  .class public auto ansi beforefieldinit MIC$
  	extends [mscorlib]System.Object
  {
    .field  private static  class [mscorlib]System.Collections.Generic.Dictionary`2<string,native int> dict
    .field  private static  class [mscorlib]System.Text.Encoding latin1

    // method line 1
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' ()  cil managed 
    {
        // Method begins at RVA 0x2050
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void object::'.ctor'()
	IL_0006:  ret 
    } // end of method MIC$::.ctor

    // method line 2
    .method public static hidebysig 
           default native int strlit (string str)  cil managed 
    {
        // Method begins at RVA 0x2058
	// Code size 66 (0x42)
	.maxstack 4
	.locals init (
		native int	V_0,
		unsigned int8[]	V_1)
	IL_0000:  ldsfld class [mscorlib]System.Collections.Generic.Dictionary`2<string,native int> MIC$::dict
	IL_0005:  ldarg.0 
	IL_0006:  ldloca.s 0
	IL_0008:  callvirt instance bool class [mscorlib]System.Collections.Generic.Dictionary`2<string, native int>::TryGetValue(!0, [out] !1&)
	IL_000d:  brfalse IL_0014

	IL_0012:  ldloc.0 
	IL_0013:  ret 
	IL_0014:  ldsfld class [mscorlib]System.Text.Encoding MIC$::latin1
	IL_0019:  ldarg.0 
	IL_001a:  callvirt instance unsigned int8[] class [mscorlib]System.Text.Encoding::GetBytes(string)
	IL_001f:  stloc.1 
	IL_0020:  ldloc.1 
	IL_0021:  ldlen 
	IL_0022:  conv.i4 
	IL_0023:  call native int class [mscorlib]System.Runtime.InteropServices.Marshal::AllocHGlobal(int32)
	IL_0028:  stloc.0 
	IL_0029:  ldloc.1 
	IL_002a:  ldc.i4.0 
	IL_002b:  ldloc.0 
	IL_002c:  ldloc.1 
	IL_002d:  ldlen 
	IL_002e:  conv.i4 
	IL_002f:  call void class [mscorlib]System.Runtime.InteropServices.Marshal::Copy(unsigned int8[], int32, native int, int32)
	IL_0034:  ldsfld class [mscorlib]System.Collections.Generic.Dictionary`2<string,native int> MIC$::dict
	IL_0039:  ldarg.0 
	IL_003a:  ldloc.0 
	IL_003b:  callvirt instance void class [mscorlib]System.Collections.Generic.Dictionary`2<string, native int>::set_Item(!0, !1)
	IL_0040:  ldloc.0 
	IL_0041:  ret 
    } // end of method MIC$::strlit

    // method line 3
    .method public static hidebysig 
           default int32 strcmp (unsigned int8* lhs, unsigned int8* rhs)  cil managed 
    {
        // Method begins at RVA 0x20a8
	// Code size 96 (0x60)
	.maxstack 3
	.locals init (
		int32	V_0)
	IL_0000:  ldarg.0 
	IL_0001:  brtrue IL_000e

	IL_0006:  ldarg.1 
	IL_0007:  brtrue IL_000e

	IL_000c:  ldc.i4.0 
	IL_000d:  ret 
	IL_000e:  ldarg.0 
	IL_000f:  brtrue IL_0016

	IL_0014:  ldc.i4.m1 
	IL_0015:  ret 
	IL_0016:  ldarg.1 
	IL_0017:  brtrue IL_001e

	IL_001c:  ldc.i4.1 
	IL_001d:  ret 
	IL_001e:  ldc.i4.0 
	IL_001f:  stloc.0 
	IL_0020:  br IL_005b

	IL_0025:  ldarg.0 
	IL_0026:  ldloc.0 
	IL_0027:  conv.i 
	IL_0028:  add 
	IL_0029:  ldind.u1 
	IL_002a:  ldarg.1 
	IL_002b:  ldloc.0 
	IL_002c:  conv.i 
	IL_002d:  add 
	IL_002e:  ldind.u1 
	IL_002f:  beq IL_004b

	IL_0034:  ldarg.0 
	IL_0035:  ldloc.0 
	IL_0036:  conv.i 
	IL_0037:  add 
	IL_0038:  ldind.u1 
	IL_0039:  ldarg.1 
	IL_003a:  ldloc.0 
	IL_003b:  conv.i 
	IL_003c:  add 
	IL_003d:  ldind.u1 
	IL_003e:  bge IL_0049

	IL_0043:  ldc.i4.m1 
	IL_0044:  br IL_004a

	IL_0049:  ldc.i4.1 
	IL_004a:  ret 
	IL_004b:  ldarg.0 
	IL_004c:  ldloc.0 
	IL_004d:  conv.i 
	IL_004e:  add 
	IL_004f:  ldind.u1 
	IL_0050:  brtrue IL_0057

	IL_0055:  ldc.i4.0 
	IL_0056:  ret 
	IL_0057:  ldloc.0 
	IL_0058:  ldc.i4.1 
	IL_0059:  add 
	IL_005a:  stloc.0 
	IL_005b:  br IL_0025

    } // end of method MIC$::strcmp

    // method line 4
    .method public static hidebysig 
           default bool relop1 (unsigned int8* l, unsigned int8* r, int32 op)  cil managed 
    {
        // Method begins at RVA 0x2114
	// Code size 114 (0x72)
	.maxstack 2
	IL_0000:  ldarg.2 
	IL_0001:  ldc.i4.1 
	IL_0002:  sub 
	IL_0003:  switch (
	  IL_0025,
	  IL_0030,
	  IL_003e,
	  IL_0049,
	  IL_0057,
	  IL_0062)
	IL_0020:  br IL_0070

	IL_0025:  ldarg.0 
	IL_0026:  ldarg.1 
	IL_0027:  call int32 class MIC$::strcmp(unsigned int8*, unsigned int8*)
	IL_002c:  ldc.i4.0 
	IL_002d:  ceq 
	IL_002f:  ret 
	IL_0030:  ldarg.0 
	IL_0031:  ldarg.1 
	IL_0032:  call int32 class MIC$::strcmp(unsigned int8*, unsigned int8*)
	IL_0037:  ldc.i4.0 
	IL_0038:  ceq 
	IL_003a:  ldc.i4.0 
	IL_003b:  ceq 
	IL_003d:  ret 
	IL_003e:  ldarg.0 
	IL_003f:  ldarg.1 
	IL_0040:  call int32 class MIC$::strcmp(unsigned int8*, unsigned int8*)
	IL_0045:  ldc.i4.0 
	IL_0046:  clt 
	IL_0048:  ret 
	IL_0049:  ldarg.0 
	IL_004a:  ldarg.1 
	IL_004b:  call int32 class MIC$::strcmp(unsigned int8*, unsigned int8*)
	IL_0050:  ldc.i4.0 
	IL_0051:  cgt 
	IL_0053:  ldc.i4.0 
	IL_0054:  ceq 
	IL_0056:  ret 
	IL_0057:  ldarg.0 
	IL_0058:  ldarg.1 
	IL_0059:  call int32 class MIC$::strcmp(unsigned int8*, unsigned int8*)
	IL_005e:  ldc.i4.0 
	IL_005f:  cgt 
	IL_0061:  ret 
	IL_0062:  ldarg.0 
	IL_0063:  ldarg.1 
	IL_0064:  call int32 class MIC$::strcmp(unsigned int8*, unsigned int8*)
	IL_0069:  ldc.i4.0 
	IL_006a:  clt 
	IL_006c:  ldc.i4.0 
	IL_006d:  ceq 
	IL_006f:  ret 
	IL_0070:  ldc.i4.0 
	IL_0071:  ret 
    } // end of method MIC$::relop1

    // method line 5
    .method public static hidebysig 
           default bool relop2 (unsigned int8* lhs, unsigned int8 rhs, int32 op)  cil managed 
    {
        // Method begins at RVA 0x2194
	// Code size 23 (0x17)
	.maxstack 3
	.locals init (
		unsigned int8*	V_0)
	IL_0000:  ldc.i4.2 
	IL_0001:  ldc.i4.1 
	IL_0002:  mul.ovf.un 
	IL_0003:  localloc 
	IL_0005:  stloc.0 
	IL_0006:  ldloc.0 
	IL_0007:  ldarg.1 
	IL_0008:  stind.i1 
	IL_0009:  ldloc.0 
	IL_000a:  ldc.i4.1 
	IL_000b:  add 
	IL_000c:  ldc.i4.0 
	IL_000d:  stind.i1 
	IL_000e:  ldarg.0 
	IL_000f:  ldloc.0 
	IL_0010:  ldarg.2 
	IL_0011:  call bool class MIC$::relop1(unsigned int8*, unsigned int8*, int32)
	IL_0016:  ret 
    } // end of method MIC$::relop2

    // method line 6
    .method public static hidebysig 
           default bool relop3 (unsigned int8 lhs, unsigned int8* rhs, int32 op)  cil managed 
    {
        // Method begins at RVA 0x21b8
	// Code size 23 (0x17)
	.maxstack 3
	.locals init (
		unsigned int8*	V_0)
	IL_0000:  ldc.i4.2 
	IL_0001:  ldc.i4.1 
	IL_0002:  mul.ovf.un 
	IL_0003:  localloc 
	IL_0005:  stloc.0 
	IL_0006:  ldloc.0 
	IL_0007:  ldarg.0 
	IL_0008:  stind.i1 
	IL_0009:  ldloc.0 
	IL_000a:  ldc.i4.1 
	IL_000b:  add 
	IL_000c:  ldc.i4.0 
	IL_000d:  stind.i1 
	IL_000e:  ldloc.0 
	IL_000f:  ldarg.1 
	IL_0010:  ldarg.2 
	IL_0011:  call bool class MIC$::relop1(unsigned int8*, unsigned int8*, int32)
	IL_0016:  ret 
    } // end of method MIC$::relop3

    // method line 7
    .method public static hidebysig 
           default bool relop4 (unsigned int8 lhs, unsigned int8 rhs, int32 op)  cil managed 
    {
        // Method begins at RVA 0x21dc
	// Code size 37 (0x25)
	.maxstack 3
	.locals init (
		unsigned int8*	V_0,
		unsigned int8*	V_1)
	IL_0000:  ldc.i4.2 
	IL_0001:  ldc.i4.1 
	IL_0002:  mul.ovf.un 
	IL_0003:  localloc 
	IL_0005:  stloc.0 
	IL_0006:  ldc.i4.2 
	IL_0007:  ldc.i4.1 
	IL_0008:  mul.ovf.un 
	IL_0009:  localloc 
	IL_000b:  stloc.1 
	IL_000c:  ldloc.0 
	IL_000d:  ldarg.0 
	IL_000e:  stind.i1 
	IL_000f:  ldloc.0 
	IL_0010:  ldc.i4.1 
	IL_0011:  add 
	IL_0012:  ldc.i4.0 
	IL_0013:  stind.i1 
	IL_0014:  ldloc.1 
	IL_0015:  ldarg.1 
	IL_0016:  stind.i1 
	IL_0017:  ldloc.1 
	IL_0018:  ldc.i4.1 
	IL_0019:  add 
	IL_001a:  ldc.i4.0 
	IL_001b:  stind.i1 
	IL_001c:  ldloc.0 
	IL_001d:  ldloc.1 
	IL_001e:  ldarg.2 
	IL_001f:  call bool class MIC$::relop1(unsigned int8*, unsigned int8*, int32)
	IL_0024:  ret 
    } // end of method MIC$::relop4

    // method line 8
    .method public static hidebysig 
           default unsigned int32 SetDiv (unsigned int32 lhs, unsigned int32 rhs)  cil managed 
    {
        // Method begins at RVA 0x220d
	// Code size 9 (0x9)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  ldarg.1 
	IL_0002:  and 
	IL_0003:  not 
	IL_0004:  ldarg.0 
	IL_0005:  ldarg.1 
	IL_0006:  or 
	IL_0007:  and 
	IL_0008:  ret 
    } // end of method MIC$::SetDiv

    // method line 9
    .method public static hidebysig 
           default bool SetIn (unsigned int32 lhs, unsigned int32 rhs)  cil managed 
    {
        // Method begins at RVA 0x2217
	// Code size 18 (0x12)
	.maxstack 8
	IL_0000:  ldc.i4.1 
	IL_0001:  ldarg.0 
	IL_0002:  ldc.i4.s 0x1f
	IL_0004:  and 
	IL_0005:  shl 
	IL_0006:  conv.i8 
	IL_0007:  ldarg.1 
	IL_0008:  conv.u8 
	IL_0009:  and 
	IL_000a:  ldc.i4.0 
	IL_000b:  conv.i8 
	IL_000c:  ceq 
	IL_000e:  ldc.i4.0 
	IL_000f:  ceq 
	IL_0011:  ret 
    } // end of method MIC$::SetIn

    // method line 10
    .method public static hidebysig 
           default void printI8 (int64 i)  cil managed 
    {
        // Method begins at RVA 0x222a
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call void class [mscorlib]System.Console::Write(int64)
	IL_0006:  ret 
    } // end of method MIC$::printI8

    // method line 11
    .method public static hidebysig 
           default void printU8 (unsigned int64 i)  cil managed 
    {
        // Method begins at RVA 0x2232
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call void class [mscorlib]System.Console::Write(unsigned int64)
	IL_0006:  ret 
    } // end of method MIC$::printU8

    // method line 12
    .method public static hidebysig 
           default void printF8 (float64 d)  cil managed 
    {
        // Method begins at RVA 0x223a
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call void class [mscorlib]System.Console::Write(float64)
	IL_0006:  ret 
    } // end of method MIC$::printF8

    // method line 13
    .method public static hidebysig 
           default void printStr (unsigned int8* s)  cil managed 
    {
        // Method begins at RVA 0x2244
	// Code size 46 (0x2e)
	.maxstack 3
	.locals init (
		int32	V_0)
	IL_0000:  ldarg.0 
	IL_0001:  brtrue IL_0007

	IL_0006:  ret 
	IL_0007:  ldc.i4.0 
	IL_0008:  stloc.0 
	IL_0009:  br IL_0012

	IL_000e:  ldloc.0 
	IL_000f:  ldc.i4.1 
	IL_0010:  add 
	IL_0011:  stloc.0 
	IL_0012:  ldarg.0 
	IL_0013:  ldloc.0 
	IL_0014:  conv.i 
	IL_0015:  add 
	IL_0016:  ldind.u1 
	IL_0017:  brtrue IL_000e

	IL_001c:  ldsfld class [mscorlib]System.Text.Encoding MIC$::latin1
	IL_0021:  ldarg.0 
	IL_0022:  ldloc.0 
	IL_0023:  callvirt instance string class [mscorlib]System.Text.Encoding::GetString(unsigned int8*, int32)
	IL_0028:  call void class [mscorlib]System.Console::Write(string)
	IL_002d:  ret 
    } // end of method MIC$::printStr

    // method line 14
    .method public static hidebysig 
           default void printCh (unsigned int8 c)  cil managed 
    {
        // Method begins at RVA 0x227e
	// Code size 8 (0x8)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  conv.u2 
	IL_0002:  call void class [mscorlib]System.Console::Write(char)
	IL_0007:  ret 
    } // end of method MIC$::printCh

    // method line 15
    .method public static hidebysig 
           default void printBool (bool b)  cil managed 
    {
        // Method begins at RVA 0x2287
	// Code size 32 (0x20)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  brfalse IL_0015

	IL_0006:  ldstr "true"
	IL_000b:  call void class [mscorlib]System.Console::Write(string)
	IL_0010:  br IL_001f

	IL_0015:  ldstr "false"
	IL_001a:  call void class [mscorlib]System.Console::Write(string)
	IL_001f:  ret 
    } // end of method MIC$::printBool

    // method line 16
    .method public static hidebysig 
           default void printSet (unsigned int32 s)  cil managed 
    {
        // Method begins at RVA 0x22a8
	// Code size 23 (0x17)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  conv.u8 
	IL_0002:  ldc.i4.2 
	IL_0003:  call string class [mscorlib]System.Convert::ToString(int64, int32)
	IL_0008:  ldc.i4.s 0x20
	IL_000a:  ldc.i4.s 0x30
	IL_000c:  callvirt instance string string::PadLeft(int32, char)
	IL_0011:  call void class [mscorlib]System.Console::Write(string)
	IL_0016:  ret 
    } // end of method MIC$::printSet

    // method line 17
    .method public static hidebysig 
           default void 'assert' (bool cond, unsigned int32 line, unsigned int8* file)  cil managed 
    {
        // Method begins at RVA 0x22c0
	// Code size 67 (0x43)
	.maxstack 5
	.locals init (
		int32	V_0)
	IL_0000:  ldarg.0 
	IL_0001:  brtrue IL_0042

	IL_0006:  ldarg.2 
	IL_0007:  brfalse IL_0042

	IL_000c:  ldc.i4.0 
	IL_000d:  stloc.0 
	IL_000e:  br IL_0017

	IL_0013:  ldloc.0 
	IL_0014:  ldc.i4.1 
	IL_0015:  add 
	IL_0016:  stloc.0 
	IL_0017:  ldarg.2 
	IL_0018:  ldloc.0 
	IL_0019:  conv.i 
	IL_001a:  add 
	IL_001b:  ldind.u1 
	IL_001c:  brtrue IL_0013

	IL_0021:  call class [mscorlib]System.IO.TextWriter class [mscorlib]System.Console::get_Error()
	IL_0026:  ldstr "assertion failed in {0} line {1}"
	IL_002b:  ldsfld class [mscorlib]System.Text.Encoding MIC$::latin1
	IL_0030:  ldarg.2 
	IL_0031:  ldloc.0 
	IL_0032:  callvirt instance string class [mscorlib]System.Text.Encoding::GetString(unsigned int8*, int32)
	IL_0037:  ldarg.1 
	IL_0038:  box [mscorlib]System.UInt32
	IL_003d:  callvirt instance void class [mscorlib]System.IO.TextWriter::WriteLine(string, object, object)
	IL_0042:  ret 
    } // end of method MIC$::assert

    // method line 18
    .method public static hidebysig 
           default void exit (int32 res)  cil managed 
    {
        // Method begins at RVA 0x230f
	// Code size 1 (0x1)
	.maxstack 8
	IL_0000:  ret 
    } // end of method MIC$::exit

    // method line 19
    .method private static hidebysig specialname rtspecialname 
           default void '.cctor' ()  cil managed 
    {
        // Method begins at RVA 0x2311
	// Code size 26 (0x1a)
	.maxstack 8
	IL_0000:  newobj instance void class [mscorlib]System.Collections.Generic.Dictionary`2<string, native int>::'.ctor'()
	IL_0005:  stsfld class [mscorlib]System.Collections.Generic.Dictionary`2<string,native int> MIC$::dict
	IL_000a:  ldstr "ISO-8859-1"
	IL_000f:  call class [mscorlib]System.Text.Encoding class [mscorlib]System.Text.Encoding::GetEncoding(string)
	IL_0014:  stsfld class [mscorlib]System.Text.Encoding MIC$::latin1
	IL_0019:  ret 
    } // end of method MIC$::.cctor

  } // end of class MIC

