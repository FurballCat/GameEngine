/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CPHYSICS_EXPORT
#           define CPHYSICS_API __declspec(dllexport)
#       else
#           define CPHYSICS_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CPHYSICS_EXPORT
#           define CPHYSICS_API __attribute__((visibility("default")))
#       else
#           define CPHYSICS_API
#       endif
#   endif
#else
#	define CPHYSICS_API
#endif
