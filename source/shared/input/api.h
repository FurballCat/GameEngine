#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef INPUT_EXPORT
#           define INPUT_API __declspec(dllexport)
#       else
#           define INPUT_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef INPUT_EXPORT
#           define INPUT_API __attribute__((visibility("default")))
#       else
#           define INPUT_API
#       endif
#   endif
#else
#	define INPUT_API
#endif
