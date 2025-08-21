MIC+.dll is implemented in C# 2.0 (i.e. MIC+.cs),
but unfortunately C# doesn't support '$' in idents.
So I disasemble the MIC+.dll created by mcs, change
the assembly and top class name to MIC$, and
recompile with ILASM
