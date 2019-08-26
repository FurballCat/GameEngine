#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef SCRIPTS_EXPORT
#           define SCRIPTS_API __declspec(dllexport)
#       else
#           define SCRIPTS_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef SCRIPTS_EXPORT
#           define SCRIPTS_API __attribute__((visibility("default")))
#       else
#           define SCRIPTS_API
#       endif
#   endif
#else
#	define SCRIPTS_API
#endif
