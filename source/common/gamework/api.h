#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef GAMEWORK_EXPORT
#           define GAMEWORK_API __declspec(dllexport)
#       else
#           define GAMEWORK_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef GAMEWORK_EXPORT
#           define GAMEWORK_API __attribute__((visibility("default")))
#       else
#           define GAMEWORK_API
#       endif
#   endif
#else
#	define GAMEWORK_API
#endif
