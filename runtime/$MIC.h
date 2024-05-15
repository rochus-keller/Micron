#ifndef _MIC_RUNTIME_
#define _MIC_RUNTIME_

#include <stdint.h>

extern int $MIC$relop1(const char* lhs, const char* rhs, int op);
extern int $MIC$relop2(const char* lhs, char rhs, int op);
extern int $MIC$relop3(char lhs, const char* rhs, int op);
extern int $MIC$relop4(char lhs, char rhs, int op);
extern int32_t $MIC$Div32( int32_t a, int32_t b );
extern int64_t $MIC$Div64( int64_t a, int64_t b );
extern int32_t $MIC$Mod32( int32_t a, int32_t b );
extern int64_t $MIC$Mod64( int64_t a, int64_t b );
extern uint32_t $MIC$SetDiv( uint32_t lhs, uint32_t rhs );
extern uint32_t $MIC$SetIn( uint32_t lhs, uint32_t rhs );
extern void $MIC$printI8(int64_t);
extern void $MIC$printU8(uint64_t);
extern void $MIC$printF8(double);
extern void $MIC$printStr(const char*);
extern void $MIC$printCh(char);
extern void $MIC$printBool(uint8_t);
extern void $MIC$printSet(uint32_t);
extern void* $MIC$alloc32(uint32_t);
extern void $MIC$free(void*);
extern void $MIC$strcopy(char*,char*);
extern void $MIC$assert(uint8_t cond, uint32_t line, const char* file);


#endif // _MIC_RUNTIME_
