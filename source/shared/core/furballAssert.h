#pragma once
#include <assert.h>

#ifdef DEBUG

    #ifdef PLATFORM_OSX
        #define ASSERT(expression) assert(expression)
    #else
        #define ASSERT(expression)
    #endif

#else
    #define ASSERT(expression)
#endif

#define FUR_ASSERT(expression) ASSERT(expression)
