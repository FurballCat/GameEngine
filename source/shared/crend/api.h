/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CREND_EXPORT
#           define CREND_API __declspec(dllexport)
#       else
#           define CREND_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CREND_EXPORT
#           define CREND_API __attribute__((visibility("default")))
#       else
#           define CREND_API
#       endif
#   endif
#else
#	define CREND_API 
#endif
