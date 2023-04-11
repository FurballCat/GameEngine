/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "ccore/types.h"
#include "api.h"

typedef struct FcAllocator FcAllocator;

typedef struct FcInputEvent
{
	f32 value;
	u16 eventID;
	u8 playerID;
	u8 deviceID;
} FcInputEvent;

typedef struct FcInputManager FcInputManager;

FcResult fcCreateInputManager(const FcAllocator* allocator, FcInputManager** inputManager);
void fcDestroyInputManager(FcInputManager* pMgr, const FcAllocator* allocator);
	
void fcInputManagerUpdate(FcInputManager* pMgr, f64 currentTime);
u32 fcInputManagerGetEvents(const FcInputManager* pMgr, FcInputEvent* pEvents, u32 capacity, u32 startIndex);

// compatible with GLFW
enum GamepadInputID
{
	// buttons
	Gamepad_cross = 0,
	Gamepad_circle,
	Gamepad_square,
	Gamepad_triangle,
	Gamepad_leftBumper,
	Gamepad_rightBumper,
	Gamepad_back,
	Gamepad_start,
	Gamepad_guide,
	Gamepad_leftThumb,
	Gamepad_rightThumb,
	Gamepad_dpadUp,
	Gamepad_dpadRight,
	Gamepad_dpadDown,
	Gamepad_dpadLeft,
	Gamepad_leftStick,
	Gamepad_rightStick,
	
	// axes
	Gamepad_leftAnalogX,
	Gamepad_leftAnalogY,
	Gamepad_rightAnalogX,
	Gamepad_rightAnalogY,
	Gamepad_leftTrigger,
	Gamepad_rightTrigger,

	Gamepad_firstAxisIndex = Gamepad_leftAnalogX,
};

#ifdef __cplusplus
}
#endif // __cplusplus
