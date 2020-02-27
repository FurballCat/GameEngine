/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CMATH_EXPORT
#           define CMATH_API __declspec(dllexport)
#       else
#           define CMATH_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CMATH_EXPORT
#           define CMATH_API __attribute__((visibility("default")))
#       else
#           define CMATH_API
#       endif
#   endif
#else
#	define CMATH_API
#endif
