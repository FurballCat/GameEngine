#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef GPU_EXPORT
#           define GPU_API __declspec(dllexport)
#       else
#           define GPU_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef GPU_EXPORT
#           define GPU_API __attribute__((visibility("default")))
#       else
#           define GPU_API
#       endif
#   endif
#else
#	define GPU_API
#endif
