/* Copyright (c) Furball Cat */

#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CGAME_EXPORT
#           define CGAME_API __declspec(dllexport)
#       else
#           define CGAME_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CGAME_EXPORT
#           define CGAME_API __attribute__((visibility("default")))
#       else
#           define CGAME_API
#       endif
#   endif
#else
#	define CGAME_API
#endif
