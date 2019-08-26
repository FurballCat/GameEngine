#pragma once

#ifdef PLATFORM_OSX

#elif PLATFORM_WINDOWS

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#undef near
#undef far

#else

#endif

//////////////////////////////////////////////////////////////////////////

#include "api.h"
#include "entryPoint.h"
