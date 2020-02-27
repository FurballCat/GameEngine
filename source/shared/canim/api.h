/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CANIM_EXPORT
#           define CANIM_API __declspec(dllexport)
#       else
#           define CANIM_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CANIM_EXPORT
#           define CANIM_API __attribute__((visibility("default")))
#       else
#           define CANIM_API
#       endif
#   endif
#else
#	define CANIM_API
#endif
