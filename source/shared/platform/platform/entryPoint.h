#pragma once

#ifdef PLATFORM_OSX

#define FUR_MAIN int main(int argc, const char * argv[])

#elif PLATFORM_WINDOWS

#define FUR_MAIN int WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR lpCmdLine, INT nCmdShow)

#else

#define FUR_MAIN 

#endif
