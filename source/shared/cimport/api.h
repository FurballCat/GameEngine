/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CIMPORT_EXPORT
#           define CIMPORT_API __declspec(dllexport)
#       else
#           define CIMPORT_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CIMPORT_EXPORT
#           define CIMPORT_API __attribute__((visibility("default")))
#       else
#           define CIMPORT_API
#       endif
#   endif
#else
#	define CIMPORT_API
#endif
