#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef PROFILER_EXPORT
#           define PROFILER_API __declspec(dllexport)
#       else
#           define PROFILER_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef PROFILER_EXPORT
#           define PROFILER_API __attribute__((visibility("default")))
#       else
#           define PROFILER_API
#       endif
#   endif
#else
#	define PROFILER_API
#endif
