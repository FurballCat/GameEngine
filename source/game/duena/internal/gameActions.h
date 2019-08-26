#pragma once

namespace input
{
	typedef uint32 InputEventID;
	struct InputEvent;
}

namespace game
{
	// input-action mapping
	enum GameplayAction
	{
		GA_MoveLeft = 0,
		GA_MoveRight,
		GA_MoveUp,
		GA_MoveDown,
		GA_Shoot,
		GA_ACTIONS_COUNT
	};
	
	struct ActionMapping
	{
		input::InputEventID m_inputID;
		GameplayAction m_action;
	};
	
	void MapInputToActionValues(ArrayView<const input::InputEvent> inputEvents, ArrayView<float> actionsPlayer1, ArrayView<float> actionsPlayer2);
}
