/* Copyright (c) Furball Cat */

#include "winHIDInput.h"

#if PLATFORM_WINDOWS

void fi_hid_input_init(fi_hid_input_t* pInput)
{

}

void fi_hid_input_update(fi_hid_input_t* pInput, double currentTime)
{

}

uint32_t fi_hid_input_get_events(const fi_hid_input_t* pInput, fi_input_event_t* pEvents, uint32_t capacity, uint32_t startIndex)
{
	return 0;
}

#endif
