#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef CORE_EXPORT
#           define CORE_API __declspec(dllexport)
#       else
#           define CORE_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef CORE_EXPORT
#           define CORE_API __attribute__((visibility("default")))
#       else
#           define CORE_API
#       endif
#   endif
#else
#	define CORE_API 
#endif
