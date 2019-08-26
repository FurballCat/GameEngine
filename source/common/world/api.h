#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef WORLD_EXPORT
#           define WORLD_API __declspec(dllexport)
#       else
#           define WORLD_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef WORLD_EXPORT
#           define WORLD_API __attribute__((visibility("default")))
#       else
#           define WORLD_API
#       endif
#   endif
#else
#	define WORLD_API
#endif
