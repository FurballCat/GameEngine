#pragma once

#define PLATFORM_TYPE_MAPPING(_namespace, _interface, _implementation)  \
    namespace _namespace  \
    {   \
        class _interface;  \
        class _implementation;    \
        inline _implementation* PlatformCast(_interface* ptr) { return reinterpret_cast<_implementation*>(ptr); } \
        inline const _implementation* PlatformCast(const _interface* ptr) { return reinterpret_cast<const _implementation*>(ptr); }  \
    }

PLATFORM_TYPE_MAPPING(platform, IWindow, WindowsWindow)
PLATFORM_TYPE_MAPPING(gpu, IGraphicsDevice, MetalGraphicsDevice)
