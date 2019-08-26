#include "pch.h"
#include "gameActions.h"
#include "input/public.h"

using namespace game;

// todo: this is a copy-paste from macInputGamepad.h
enum GamepadInputID
{
	Gamepad_faceButtonBottom = 0,
	Gamepad_faceButtonRight,
	Gamepad_faceButtonLeft,
	Gamepad_faceButtonTop,
	Gamepad_leftShoulder,
	Gamepad_rightShoulder,
	Gamepad_specialRight,
	Gamepad_specialLeft,
	Gamepad_leftThumb,
	Gamepad_rightThumb,
	Gamepad_leftTriggerThreshold,
	Gamepad_rightTriggerThreshold,
	Gamepad_dpadUp,
	Gamepad_dpadDown,
	Gamepad_dpadLeft,
	Gamepad_dpadRight,
	Gamepad_leftStickUp,
	Gamepad_leftStickDown,
	Gamepad_leftStickLeft,
	Gamepad_leftStickRight,
	Gamepad_rightStickUp,
	Gamepad_rightStickDown,
	Gamepad_rightStickLeft,
	Gamepad_rightStickRight,
	
	Gamepad_leftAnalogX,
	Gamepad_leftAnalogY,
	Gamepad_rightAnalogX,
	Gamepad_rightAnalogY,
	Gamepad_leftTrigger,
	Gamepad_rightTrigger,
};

// Action mapping
ActionMapping g_actionMapping[GA_ACTIONS_COUNT] =
{
	{Gamepad_leftAnalogY, GA_MoveUp},
	{Gamepad_dpadUp, GA_MoveDown},
	{Gamepad_leftAnalogX, GA_MoveLeft},
	{Gamepad_dpadUp, GA_MoveRight},
	{Gamepad_rightTriggerThreshold, GA_Shoot}
};

void game::MapInputToActionValues(ArrayView<const input::InputEvent> inputEvents, ArrayView<float> actionsPlayer1, ArrayView<float> actionsPlayer2)
{
	for(uint32 i=0; i<inputEvents.Size(); ++i)
	{
		const input::InputEvent& evt = inputEvents[i];
		for(uint32 a=0; a<GA_ACTIONS_COUNT; ++a)
		{
			if(g_actionMapping[a].m_inputID == evt.m_id)
			{
				if(evt.m_user == 0)
					actionsPlayer1[g_actionMapping[a].m_action] = evt.m_value;
				else if(evt.m_user == 1)
					actionsPlayer2[g_actionMapping[a].m_action] = evt.m_value;
				break;
			}
		}
	}
}
