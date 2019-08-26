#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef ANIMATION_EXPORT
#           define ANIMATION_API __declspec(dllexport)
#       else
#           define ANIMATION_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef ANIMATION_EXPORT
#           define ANIMATION_API __attribute__((visibility("default")))
#       else
#           define ANIMATION_API
#       endif
#   endif
#else
#	define ANIMATION_API
#endif
