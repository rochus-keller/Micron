#ifndef _MIC_RUNTIME_
#define _MIC_RUNTIME_

#include <stdint.h>

extern int $MIC$relop1(const char* lhs, const char* rhs, int op); // 1
extern int $MIC$relop2(const char* lhs, char rhs, int op); // 2
extern int $MIC$relop3(char lhs, const char* rhs, int op); // 3
extern int $MIC$relop4(char lhs, char rhs, int op); // 4
extern uint32_t $MIC$SetDiv( uint32_t lhs, uint32_t rhs ); // 5
extern uint32_t $MIC$SetIn( uint32_t lhs, uint32_t rhs ); // 6
extern void $MIC$printI8(int64_t); // 7
extern void $MIC$printU8(uint64_t); // 8
extern void $MIC$printF8(double); // 9
extern void $MIC$printStr(const char*); // 10
extern void $MIC$printCh(char); // 11
extern void $MIC$printBool(uint8_t); // 12
extern void $MIC$printSet(uint32_t); // 13
extern void $MIC$strcopy(char*,char*); // 14
extern void $MIC$assert(uint8_t cond, uint32_t line, const char* file); // 15


#endif // _MIC_RUNTIME_
