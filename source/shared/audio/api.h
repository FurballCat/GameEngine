#pragma once

#ifdef DLL_ENABLED
#   ifdef PLATFORM_WINDOWS
#       ifdef AUDIO_EXPORT
#           define AUDIO_API __declspec(dllexport)
#       else
#           define AUDIO_API __declspec(dllimport)
#       endif
#   elif defined(PLATFORM_OSX)
#       ifdef AUDIO_EXPORT
#           define AUDIO_API __attribute__((visibility("default")))
#       else
#           define AUDIO_API
#       endif
#   endif
#else
#	define AUDIO_API
#endif
