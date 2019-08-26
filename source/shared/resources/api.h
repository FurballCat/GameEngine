#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef RESOURCES_EXPORT
#           define RESOURCES_API __declspec(dllexport)
#       else
#           define RESOURCES_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef RESOURCES_EXPORT
#           define RESOURCES_API __attribute__((visibility("default")))
#       else
#           define RESOURCES_API
#       endif
#   endif
#else
#	define RESOURCES_API
#endif
