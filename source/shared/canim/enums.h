/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

typedef enum fm_axis_t
{
	FM_AXIS_X = 0,
	FM_AXIS_Y,
	FM_AXIS_Z,
	FM_AXIS_NEG_X,
	FM_AXIS_NEG_Y,
	FM_AXIS_NEG_Z,
} fm_axis_t;

typedef enum fa_mask_t
{
	FA_MASK_NONE = 0,
	FA_MASK_UPPER_BODY,
	FA_MASK_FACE,
	FA_MASK_HANDS,
} fa_mask_t;

#ifdef __cplusplus
}
#endif // __cplusplus
