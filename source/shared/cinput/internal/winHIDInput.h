/* Copyright (c) Furball Cat */

#pragma once

#if PLATFORM_WINDOWS

#include "input.h"
#include <stdbool.h>

struct TimeInfo;

#define MAX_CONTROLLERS 16
#define MAX_CONTROLLER_BUTTON_MAPPINGS 24
#define MAX_PENDING_EVENTS 128
#define MAX_DEVICE_ELEMENTS 64

typedef struct ToDoImplement
{
	int stuff;
} ToDoImplement;

typedef struct fi_hid_element_info_t
{
	ToDoImplement m_elementRef;
	ToDoImplement m_type;
	uint16_t m_usagePage;
	uint16_t m_usage;
	int32_t m_minValue;
	int32_t m_maxValue;
} fi_hid_element_info_t;

typedef struct fi_device_info_t
{
	ToDoImplement m_deviceRef;
	fi_hid_element_info_t m_elements[MAX_DEVICE_ELEMENTS];
	uint32_t m_numElements;
	
	int8_t m_buttonMappings[MAX_CONTROLLER_BUTTON_MAPPINGS];
	uint16_t m_leftAnalogXMapping;
	uint16_t m_leftAnalogYMapping;
	uint16_t m_leftTriggerAnalogMapping;
	uint16_t m_rightAnalogXMapping;
	uint16_t m_rightAnalogYMapping;
	uint16_t m_rightTriggerAnalogMapping;
} fi_device_info_t;

typedef struct fi_controller_state_t
{
	fi_device_info_t m_device;
	bool m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
	int32_t m_leftAnalogX;
	int32_t m_leftAnalogY;
	int32_t m_rightAnalogX;
	int32_t m_rightAnalogY;
	int32_t m_leftTriggerAnalog;
	int32_t m_rightTriggerAnalog;
	bool m_buttonStates[MAX_CONTROLLER_BUTTON_MAPPINGS];
	float m_nextRepeatTime[MAX_CONTROLLER_BUTTON_MAPPINGS];
	
	uint8_t m_controllerId;
} fi_controller_state_t;

typedef struct fi_hid_input_t
{
	fi_controller_state_t m_controllers[MAX_CONTROLLERS];
	
	uint32_t m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
	
	float m_initialButtonRepeatDelay;
	float m_buttonRepeatDelay;
	
	bool m_isGamepadAttached;
	
	ToDoImplement m_hidManager;
	
	fi_input_event_t m_pendingEvents[MAX_PENDING_EVENTS];
	uint32_t m_numPendingEvents;
} fi_hid_input_t;

void fi_hid_input_init(fi_hid_input_t* pInput);
void fi_hid_input_update(fi_hid_input_t* pInput, double currentTime);
uint32_t fi_hid_input_get_events(const fi_hid_input_t* pInput, fi_input_event_t* pEvents, uint32_t capacity, uint32_t startIndex);

#endif
