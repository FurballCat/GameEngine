#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef MATH_EXPORT
#           define MATH_API __declspec(dllexport)
#       else
#           define MATH_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef MATH_EXPORT
#           define MATH_API __attribute__((visibility("default")))
#       else
#           define MATH_API
#       endif
#   endif
#else
#	define MATH_API
#endif
