/* Copyright (c) Furball Cat */

#include "winHIDInput.h"
#include "glfw/glfw3.h"

#if PLATFORM_WINDOWS

void fi_hid_input_init(fi_hid_input_t* pInput)
{
	pInput->m_numPendingEvents = 0;
}

void fi_hid_input_update(fi_hid_input_t* pInput, f64 currentTime)
{
	const i32 maxControllers = MAX_CONTROLLERS > GLFW_JOYSTICK_LAST ? GLFW_JOYSTICK_LAST : MAX_CONTROLLERS;

	pInput->m_numPendingEvents = 0;

	for (int i = GLFW_JOYSTICK_1; i <= maxControllers; i++)
	{
		// is pad active
		if (!glfwJoystickPresent(i))
			continue;
		
		GLFWgamepadstate next;

		// get current state of this pad
		if (!glfwGetGamepadState(i, &next))
			continue;

		const GLFWgamepadstate* prev = &pInput->m_controllers[i];

		// collect axes
		for (i32 i = 0; i <= GLFW_GAMEPAD_AXIS_LAST; ++i)
		{
			if (prev->axes[i] != next.axes[i])
			{
				f32 value = next.axes[i];

				// convert -1..+1 on triggers to 0..1
				if (i == GLFW_GAMEPAD_AXIS_LEFT_TRIGGER || i == GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER)
				{
					value = (value + 1.0f) / 2.0f;
				}

				i32 idx = pInput->m_numPendingEvents;
				pInput->m_pendingEvents[idx].deviceID = i;
				pInput->m_pendingEvents[idx].playerID = i;
				pInput->m_pendingEvents[idx].value = value;
				pInput->m_pendingEvents[idx].eventID = Gamepad_firstAxisIndex + i;
				pInput->m_numPendingEvents++;
			}
		}

		// collect buttons
		for (i32 i = 0; i <= GLFW_GAMEPAD_BUTTON_LAST; ++i)
		{
			if (prev->buttons[i] != next.buttons[i])
			{
				i32 idx = pInput->m_numPendingEvents;
				pInput->m_pendingEvents[idx].deviceID = i;
				pInput->m_pendingEvents[idx].playerID = i;
				pInput->m_pendingEvents[idx].value = next.buttons[i] == GLFW_PRESS ? 1.0f : 0.0f;
				pInput->m_pendingEvents[idx].eventID = i;
				pInput->m_numPendingEvents++;
			}
		}

		pInput->m_controllers[i] = next;
	}
}

u32 fi_hid_input_get_events(const fi_hid_input_t* pInput, fi_input_event_t* pEvents, u32 capacity, u32 startIndex)
{
	i32 count = 0;

	for (i32 i = startIndex; i < pInput->m_numPendingEvents; ++i)
	{
		if (i >= capacity)
			break;

		pEvents[i] = pInput->m_pendingEvents[i];
		count++;
	}

	return count;
}

#endif
