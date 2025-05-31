#include <time.h>

#if defined(_WIN32) && !defined(__GNUC__)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

// Source: https://stackoverflow.com/questions/10905892/equivalent-of-gettimeday-for-windows/26085827

// MSVC defines this in winsock2.h!?
typedef struct timeval {
    long tv_sec;
    long tv_usec;
} timeval;

int gettimeofday(struct timeval * tp, struct timezone * tzp)
{
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
    // until 00:00:00 January 1, 1970 
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return 0;
}
#else
#ifndef _USE_CPU_TIME_
#include <sys/time.h>
#endif
#endif

static struct timeval start;

int Input$Time()
{
#ifdef _USE_CPU_TIME_
	clock_t now = clock();
	if( now < 0 )
		return 0;
	return now * 1000000 / CLOCKS_PER_SEC;
#else
    static struct timeval now;
    gettimeofday(&now, 0);
    const long seconds = now.tv_sec - start.tv_sec;
    const long microseconds = now.tv_usec - start.tv_usec;
    return seconds*1000000 + microseconds;
#endif
}

int Input$Available()
{
	return 0; // TODO
}

void Input$Read(char* ch)
{
    // TODO
}

static int _w = 0, _h = 0;

void Input$SetMouseLimits(int w, int h)
{
	// TODO
}

void Input$Mouse( int* keys, int* x, int* y)
{
	// TODO
}

void Input$begin$()
{
}


