proc Interface1!Point.Move
     0: ldarg_p 0
     1: ldarg_i4 0
     2: stfld_i4 0
     3: ldarg_p 0
     4: ldarg_i4 0
     5: stfld_i4 4
proc Interface1!begin$
     0: ldvara 0
     1: ldc_i4 12
     2: ldc_i4 34
     3: callinst Interface1!Point.Move
     4: ldvara 0
     5: ldfld_i4 4
     6: conv_i8_i4
     7: call MIC$!printI8
     8: ldc_i4 10
     9: call MIC$!printCh
