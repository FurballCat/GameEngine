/* Copyright (c) Furball Cat */

#include "input.h"
#include "ccore/public.h"

#if PLATFORM_OSX
#include "macHIDInput.h"
#elif PLATFORM_WINDOWS
#include "winHIDInput.h"
#else
	#error No HID input implementation for this platform
#endif

typedef struct FcInputManager
{
	FcInputHID gamepad;
} FcInputManager;

FcInputManager* fcInputManagerCreate(FcAllocator* allocator)
{
	FcInputManager* pMgr = FUR_ALLOC_AND_ZERO(sizeof(FcInputManager), 0, FC_MEMORY_SCOPE_INPUT, allocator);
	
	fcInputHIDInit(&pMgr->gamepad);
	
	return pMgr;
}

void fcInputManagerRelease(FcInputManager* pMgr, FcAllocator* allocator)
{
	FUR_FREE(pMgr, allocator);
}

void fcInputManagerUpdate(FcInputManager* pMgr, f64 currentTime)
{
	fcInputHIDUpdate(&pMgr->gamepad, currentTime);
}

u32 fcInputManagerGetEvents(const FcInputManager* pMgr, FcInputEvent* pEvents, u32 capacity, u32 startIndex)
{
	return fcInputHIDGetEvents(&pMgr->gamepad, pEvents, capacity, startIndex);
}
