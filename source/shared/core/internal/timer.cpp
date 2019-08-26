#include "pch.h"
#include "timer.h"

#ifdef PLATFORM_OSX
#include <mach/mach_time.h>
#elif PLATFORM_WINDOWS
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

Timer Timer::Tick()
{
	Timer t;
	
#ifdef PLATFORM_OSX
	mach_timebase_info_data_t rate_nsec;
	mach_timebase_info(&rate_nsec);
	t.m_rate = 1000000000LL * rate_nsec.numer / rate_nsec.denom;
	t.m_last = mach_absolute_time();
#elif PLATFORM_WINDOWS
	LARGE_INTEGER tmp{ 0 };
 
	ASSERT( QueryPerformanceFrequency( &tmp ) );
	t.m_rate = tmp.QuadPart;
	ASSERT( QueryPerformanceCounter  ( &tmp ) );
	t.m_last = tmp.QuadPart;
#endif
	
	t.m_start = t.m_last;
	t.m_end = 0.0;
	
	return t;
}

double Timer::Tock()
{
	uint64 now;
	
#ifdef PLATFORM_OSX
	now = mach_absolute_time();
#elif PLATFORM_WINDOWS
	LARGE_INTEGER tmp{ 0 };
	ASSERT( QueryPerformanceCounter( &tmp ) );
	now = tmp.QuadPart;
#endif
	
	m_end = now;
	
	double delta = ( now - m_last) / (double)m_rate;
	m_last = now;
	return delta;
}

double Timer::AbsoluteTime() const
{
	return (m_end - m_start) / (double)m_rate;
}
