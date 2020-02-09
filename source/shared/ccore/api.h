/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CCORE_EXPORT
#           define CCORE_API __declspec(dllexport)
#       else
#           define CCORE_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CCORE_EXPORT
#           define CCORE_API __attribute__((visibility("default")))
#       else
#           define CCORE_API
#       endif
#   endif
#else
#	define CCORE_API
#endif
