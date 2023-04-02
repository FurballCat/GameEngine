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

typedef struct FcInputHID
{
	GLFWgamepadstate m_controllers[MAX_CONTROLLERS];
	
	u32 m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
	
	f32 m_initialButtonRepeatDelay;
	f32 m_buttonRepeatDelay;
	
	FcInputEvent m_pendingEvents[MAX_PENDING_EVENTS];
	u32 m_numPendingEvents;
} FcInputHID;

void fcInputHIDInit(FcInputHID* pInput);
void fcInputHIDUpdate(FcInputHID* pInput, f64 currentTime);
u32 fcInputHIDGetEvents(const FcInputHID* pInput, FcInputEvent* pEvents, u32 capacity, u32 startIndex);

#endif
