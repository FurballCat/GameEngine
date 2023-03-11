/* Copyright (c) Furball Cat */

#pragma once

#if PLATFORM_WINDOWS

#include "input.h"
#include <stdbool.h>
#include "glfw/glfw3.h"

struct TimeInfo;

#define MAX_CONTROLLERS 16
#define MAX_CONTROLLER_BUTTON_MAPPINGS 24
#define MAX_PENDING_EVENTS 128
#define MAX_DEVICE_ELEMENTS 64

typedef struct fi_hid_input_t
{
	GLFWgamepadstate m_controllers[MAX_CONTROLLERS];
	
	uint32_t m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
	
	float m_initialButtonRepeatDelay;
	float m_buttonRepeatDelay;
	
	fi_input_event_t m_pendingEvents[MAX_PENDING_EVENTS];
	uint32_t m_numPendingEvents;
} fi_hid_input_t;

void fi_hid_input_init(fi_hid_input_t* pInput);
void fi_hid_input_update(fi_hid_input_t* pInput, double currentTime);
uint32_t fi_hid_input_get_events(const fi_hid_input_t* pInput, fi_input_event_t* pEvents, uint32_t capacity, uint32_t startIndex);

#endif
