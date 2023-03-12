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

typedef struct fi_input_manager_t
{
	fi_hid_input_t gamepad;
} fi_input_manager_t;

fi_input_manager_t* fi_input_manager_create(fc_alloc_callbacks_t* pAllocCallbacks)
{
	fi_input_manager_t* pMgr = FUR_ALLOC_AND_ZERO(sizeof(fi_input_manager_t), 0, FC_MEMORY_SCOPE_INPUT, pAllocCallbacks);
	
	fi_hid_input_init(&pMgr->gamepad);
	
	return pMgr;
}

void fi_input_manager_release(fi_input_manager_t* pMgr, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(pMgr, pAllocCallbacks);
}

void fi_update_input_manager(fi_input_manager_t* pMgr, f64 currentTime)
{
	fi_hid_input_update(&pMgr->gamepad, currentTime);
}

u32 fi_get_input_events(const fi_input_manager_t* pMgr, fi_input_event_t* pEvents, u32 capacity, u32 startIndex)
{
	return fi_hid_input_get_events(&pMgr->gamepad, pEvents, capacity, startIndex);
}
