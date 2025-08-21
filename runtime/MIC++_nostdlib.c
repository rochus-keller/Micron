// Forward declaration for the string length calculation
static unsigned int string_length(const char* str);

#if defined(__linux__) || defined(__APPLE__)

// defined in ECS runtime
extern long sys_write(int fd, const void* buf, int count);
extern void sys_exit(int status);
extern int sys_close(int fd);

static void console_write(const char* str) {
    sys_write(1, str, string_length(str)); // 1 is the file descriptor for stdout, 2 for stderr
}

#else
#error "Unsupported platform: No implementation for console_write."
#endif

static int my_strcmp(const char *s1, const char *s2) {
    while (*s1 && (*s1 == *s2)) {
        s1++;
        s2++;
    }
    return *(const unsigned char*)s1 - *(const unsigned char*)s2;
}

// A minimal implementation of strlen since <string.h> is not allowed.
static unsigned int string_length(const char* str) {
    unsigned int len = 0;
    while (str[len] != '\0') {
        len++;
    }
    return len;
}

// Helper to reverse a string in place, as numbers are generated backwards.
static void reverse_string(char* str) {
    unsigned int len = string_length(str);
    if (len == 0) return;
    char* start = str;
    char* end = str + len - 1;
    while (start < end) {
        char temp = *start;
        *start = *end;
        *end = temp;
        start++;
        end--;
    }
}

/**
 * @brief Prints a 64-bit unsigned integer to the console.
 * @param u The unsigned long long to print.
 */
void MIC$$printU8(unsigned long long u) {
    char buffer[21]; // A 64-bit unsigned int has max 20 digits. +1 for null terminator.
    int i = 0;

    if (u == 0) {
        console_write("0");
        return;
    }

    // Extract digits in reverse order
    while (u > 0) {
        buffer[i++] = (u % 10) + '0';
        u /= 10;
    }
    buffer[i] = '\0';

    // Reverse the string and print
    reverse_string(buffer);
    console_write(buffer);
}

/**
 * @brief Prints a 64-bit signed integer to the console.
 * @param i The long long to print.
 */
void MIC$$printI8(long long i) {
    // Handle the most negative number, whose absolute value cannot be represented
    // as a positive long long.
    if (i == -9223372036854775807LL - 1) {
        console_write("-9223372036854775808");
        return;
    }
    
    if (i == 0) {
        console_write("0");
        return;
    }

    if (i < 0) {
        console_write("-");
        i = -i; // Now safe to negate
    }

    // Reuse the unsigned printing logic
    MIC$$printU8((unsigned long long)i);
}

/**
 * @brief Prints a 64-bit double-precision float to the console.
 * @note This is a simplified implementation with fixed precision.
 * @param d The double to print.
 */
void MIC$$printF8(double d) {
    // Handle negative numbers
    if (d < 0) {
        console_write("-");
        d = -d;
    }

    // Extract and print the integer part
    unsigned long long int_part = (unsigned long long)d;
    MIC$$printU8(int_part);
    console_write(".");

    // Extract the fractional part
    double frac_part = d - (double)int_part;

    // Define precision (e.g., 6 decimal places)
    const int precision = 6;
    for (int i = 0; i < precision; ++i) {
        frac_part *= 10;
        int digit = (int)frac_part;
        char digit_char[2] = { (char)(digit + '0'), '\0' };
        console_write(digit_char);
        frac_part -= digit;
    }
}

int MIC$$relop1(const char* l, const char* r, int op)
{
	switch( op )
	{
	case 1: // EQ
		return my_strcmp(l,r) == 0;
	case 2: // NEQ
		return my_strcmp(l,r) != 0;
	case 3: // LT
		return my_strcmp(l,r) < 0;
	case 4: // LEQ
		return my_strcmp(l,r) <= 0;
	case 5: // GT
		return my_strcmp(l,r) > 0;
	case 6: // GEQ
		return my_strcmp(l,r) >= 0;
	}
	return 0;
}

int MIC$$relop2(const char* lhs, char rhs, int op)
{
	char ch[2] = "x";
	ch[0] = rhs;
	return MIC$$relop1(lhs,ch,op);
}

int MIC$$relop3(char lhs, const char* rhs, int op)
{
	char ch[2] = "x";
	ch[0] = lhs;
	return MIC$$relop1(ch,rhs,op);
}

int MIC$$relop4(char lhs, char rhs, int op)
{
	char l[2] = "x";
	l[0] = lhs;
	char r[2] = "x";
	r[0] = rhs;
	return MIC$$relop1(l,r,op);
}

unsigned int MIC$$SetDiv( unsigned int lhs, unsigned int rhs )
{
    return ~( lhs & rhs ) & ( lhs | rhs );
}

unsigned int MIC$$SetIn( unsigned int lhs, unsigned int rhs )
{
    return ((1<<lhs)&rhs) != 0;
}

void MIC$$printCh(char c)
{
    char buf[2] = { 0,0 };
    buf[0] = c;
	console_write(buf);
}

void MIC$$printBool(unsigned char b)
{
	console_write( b ? "true" : "false" );
}

void MIC$$printSet(unsigned int s)
{
	MIC$$printU8(s);
}

void MIC$$assert(unsigned char cond, unsigned int line, const char* file)
{
    if(!cond)
    {
        console_write("assertion failed in ");
        console_write(file);
        console_write(" line ");
        MIC$$printU8(line);
        console_write("\n");
    }
	sys_exit(cond);
}

void MIC$$exit(int res)
{
    sys_close(1);
	sys_exit(res);
}

void MIC$$printStr(char* s)
{
    console_write(s);
}

