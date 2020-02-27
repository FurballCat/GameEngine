/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>
	
typedef struct fm_xform fm_xform;

typedef struct fa_rig_t
{
	uint64_t* boneNameHashes;
	int16_t* parents;
	fm_xform* refPose;
} fa_rig_t;

typedef struct fa_anim_clip_key_t
{
	uint16_t keyTime;
	uint16_t idxBoneAndChannel;	// 2 bits for channel
	uint16_t keyData[3];
} fa_anim_clip_key_t;

typedef struct fa_anim_clip_t
{
	fa_anim_clip_key_t* keys;
	uint32_t duration;
	uint32_t numKeys;
} fa_anim_clip_t;

static inline uint16_t fa_anim_clip_key_get_bone_index(const fa_anim_clip_key_t* key)
{
	return key->idxBoneAndChannel & 0x3fff;
}

static inline uint16_t fa_anim_clip_key_get_channel(const fa_anim_clip_key_t* key)
{
	return (key->idxBoneAndChannel & 0xc000) >> 14;
}
	
#ifdef __cplusplus
}
#endif // __cplusplus
