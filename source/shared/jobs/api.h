#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef JOBS_EXPORT
#           define JOBS_API __declspec(dllexport)
#       else
#           define JOBS_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef JOBS_EXPORT
#           define JOBS_API __attribute__((visibility("default")))
#       else
#           define JOBS_API
#       endif
#   endif
#else
#	define JOBS_API
#endif
