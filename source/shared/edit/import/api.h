#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef IMPORT_EXPORT
#           define IMPORT_API __declspec(dllexport)
#       else
#           define IMPORT_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef IMPORT_EXPORT
#           define IMPORT_API __attribute__((visibility("default")))
#       else
#           define IMPORT_API
#       endif
#   endif
#else
#	define IMPORT_API
#endif
