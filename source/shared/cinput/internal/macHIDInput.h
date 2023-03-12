/* Copyright (c) Furball Cat */

#pragma once

#if PLATFORM_OSX

#include <IOKit/hid/IOHIDLib.h>
#include "input.h"

struct TimeInfo;

#define MAX_CONTROLLERS 16
#define MAX_CONTROLLER_BUTTON_MAPPINGS 24
#define MAX_PENDING_EVENTS 128
#define MAX_DEVICE_ELEMENTS 64

typedef struct fi_hid_element_info_t
{
	IOHIDElementRef m_elementRef;
	IOHIDElementType m_type;
	u16 m_usagePage;
	u16 m_usage;
	i32 m_minValue;
	i32 m_maxValue;
} fi_hid_element_info_t;

typedef struct fi_device_info_t
{
	IOHIDDeviceRef m_deviceRef;
	fi_hid_element_info_t m_elements[MAX_DEVICE_ELEMENTS];
	u32 m_numElements;
	
	int8_t m_buttonMappings[MAX_CONTROLLER_BUTTON_MAPPINGS];
	u16 m_leftAnalogXMapping;
	u16 m_leftAnalogYMapping;
	u16 m_leftTriggerAnalogMapping;
	u16 m_rightAnalogXMapping;
	u16 m_rightAnalogYMapping;
	u16 m_rightTriggerAnalogMapping;
} fi_device_info_t;

typedef struct fi_controller_state_t
{
	fi_device_info_t m_device;
	bool m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
	i32 m_leftAnalogX;
	i32 m_leftAnalogY;
	i32 m_rightAnalogX;
	i32 m_rightAnalogY;
	i32 m_leftTriggerAnalog;
	i32 m_rightTriggerAnalog;
	bool m_buttonStates[MAX_CONTROLLER_BUTTON_MAPPINGS];
	f32 m_nextRepeatTime[MAX_CONTROLLER_BUTTON_MAPPINGS];
	
	u8 m_controllerId;
} fi_controller_state_t;

typedef struct fi_hid_input_t
{
	fi_controller_state_t m_controllers[MAX_CONTROLLERS];
	
	u32 m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
	
	f32 m_initialButtonRepeatDelay;
	f32 m_buttonRepeatDelay;
	
	bool m_isGamepadAttached;
	
	IOHIDManagerRef m_hidManager;
	
	fi_input_event_t m_pendingEvents[MAX_PENDING_EVENTS];
	u32 m_numPendingEvents;
} fi_hid_input_t;

void fi_hid_input_init(fi_hid_input_t* pInput);
void fi_hid_input_update(fi_hid_input_t* pInput, f64 currentTime);
u32 fi_hid_input_get_events(const fi_hid_input_t* pInput, fi_input_event_t* pEvents, u32 capacity, u32 startIndex);

#endif
