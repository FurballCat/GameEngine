#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef MEMORY_EXPORT
#           define MEMORY_API __declspec(dllexport)
#       else
#           define MEMORY_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef MEMORY_EXPORT
#           define MEMORY_API __attribute__((visibility("default")))
#       else
#           define MEMORY_API
#       endif
#   endif
#else
#	define MEMORY_API
#endif
