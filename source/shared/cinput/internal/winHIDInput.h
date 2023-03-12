/* Copyright (c) Furball Cat */

#pragma once

#if PLATFORM_WINDOWS

#include "ccore/types.h"
#include "input.h"
#include "glfw/glfw3.h"

struct TimeInfo;

#define MAX_CONTROLLERS 16
#define MAX_CONTROLLER_BUTTON_MAPPINGS 24
#define MAX_PENDING_EVENTS 128
#define MAX_DEVICE_ELEMENTS 64

typedef struct fi_hid_input_t
{
	GLFWgamepadstate m_controllers[MAX_CONTROLLERS];
	
	u32 m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
	
	f32 m_initialButtonRepeatDelay;
	f32 m_buttonRepeatDelay;
	
	fi_input_event_t m_pendingEvents[MAX_PENDING_EVENTS];
	u32 m_numPendingEvents;
} fi_hid_input_t;

void fi_hid_input_init(fi_hid_input_t* pInput);
void fi_hid_input_update(fi_hid_input_t* pInput, f64 currentTime);
u32 fi_hid_input_get_events(const fi_hid_input_t* pInput, fi_input_event_t* pEvents, u32 capacity, u32 startIndex);

#endif
