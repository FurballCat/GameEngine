/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CINPUT_EXPORT
#           define CINPUT_API __declspec(dllexport)
#       else
#           define CINPUT_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CINPUT_EXPORT
#           define CINPUT_API __attribute__((visibility("default")))
#       else
#           define CINPUT_API
#       endif
#   endif
#else
#	define CINPUT_API
#endif
