#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef MACPLATFORM_EXPORT
#           define PLATFORM_API __declspec(dllexport)
#       else
#           define PLATFORM_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef MACPLATFORM_EXPORT
#           define PLATFORM_API __attribute__((visibility("default")))
#       else
#           define PLATFORM_API
#       endif
#   endif
#else
#	define SCRIPTS_API
#endif
