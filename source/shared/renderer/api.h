#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef RENDERER_EXPORT
#           define RENDERER_API __declspec(dllexport)
#       else
#           define RENDERER_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef RENDERER_EXPORT
#           define RENDERER_API __attribute__((visibility("default")))
#       else
#           define RENDERER_API
#       endif
#   endif
#else
#	define RENDERER_API
#endif
