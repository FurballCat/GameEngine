/* Copyright (c) 2016-2022 Furball Cat */

#include "animClip.h"
#include "pose.h"
#include "ccore/public.h"
#include "ccore/serialize.h"
#include "cmath/public.h"

void fa_anim_clip_release(fa_anim_clip_t* clip, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(clip->curves, pAllocCallbacks);
	FUR_FREE(clip->dataKeys, pAllocCallbacks);
	FUR_FREE(clip, pAllocCallbacks);
}

float fa_decompress_float_minus_one_plus_one(uint16_t value)
{
	return (((float)value) / 65535.0f) * 2.0f - 1.0f;
}

uint16_t fa_compress_float_minus_one_plus_one(float value)
{
	return (uint16_t)(((value + 1.0f) / 2.0f) * 65535.0f);
}

void fm_vec3_to_16bit(const fm_vec3* v, uint16_t* b)
{
	b[0] = fa_compress_float_minus_one_plus_one(v->x);
	b[1] = fa_compress_float_minus_one_plus_one(v->y);
	b[2] = fa_compress_float_minus_one_plus_one(v->z);
}

void fm_16bit_to_vec3(const uint16_t* b, fm_vec3* v)
{
	v->x = fa_decompress_float_minus_one_plus_one(b[0]);
	v->y = fa_decompress_float_minus_one_plus_one(b[1]);
	v->z = fa_decompress_float_minus_one_plus_one(b[2]);
}

const float Km  = 4.0*(0.4142135679721832275390625); // 4(sqrt(2)-1)
const float Khf = 2.414213657379150390625;           // sqrt(2)+1 = 1/(sqrt(2)-1)
const float Khi = 0.17157287895679473876953125;      // 3-2sqrt(2)

void quat_fhm(fm_quat q, fm_vec3* v)
{
	float s = Khf / (1.0 + q.r + sqrt(2.0 + 2.0 * q.r));
	
	v->x = q.i * s;
	v->y = q.j * s;
	v->z = q.k * s;
}

fm_quat quat_ihm(const fm_vec3* v)
{
	float d = Khi * fm_vec3_dot(v, v);
	float a = (1.0+d);
	float b = (1.0-d)*Km;
	float c = 1.0/(a*a);
	fm_quat q;
	
	float bc = b * c;
	
	q.i = v->x * bc;
	q.j = v->y * bc;
	q.k = v->z * bc;
	q.r = (1.0 + d * (d - 6.0)) * c;
	
	return q;
}

void quat_fhm_16bit(fm_quat q, uint16_t* v)
{
	fm_vec3 vec;
	quat_fhm(q, &vec);
	fm_vec3_to_16bit(&vec, v);
}

fm_quat quat_ihm_16bit(const uint16_t* b)
{
	fm_vec3 vec;
	fm_16bit_to_vec3(b, &vec);
	return quat_ihm(&vec);
}

const float c_posRange = 20.0f;

void vec4_com_16bit(fm_vec4 v, uint16_t* b)
{
	fm_vec3 vec = {v.x / c_posRange, v.y / c_posRange, v.z / c_posRange};
	fm_vec3_to_16bit(&vec, b);
}

fm_vec4 vec4_decom_16bit(const uint16_t* v)
{
	fm_vec3 vec;
	fm_16bit_to_vec3(v, &vec);
	
	fm_vec4 res;
	res.x = vec.x * c_posRange;
	res.y = vec.y * c_posRange;
	res.z = vec.z * c_posRange;
	res.w = 0.0f;
	
	return res;
}

void fa_decompress_rotation_key(const fa_anim_curve_key_t* key, fm_quat* rot)
{
	const uint16_t* keyData = key->keyData;
	*rot = quat_ihm_16bit(keyData);
}

void fa_decompress_position_key(const fa_anim_curve_key_t* key, fm_vec4* pos)
{
	const uint16_t* keyData = key->keyData;
	*pos = vec4_decom_16bit(keyData);
}

float fa_decompress_key_time(const uint16_t time)
{
	return ((float)time) / 24.0f;
}

void fa_anim_curve_sample(const fa_anim_curve_t* curve, float time, bool asAdditive, fm_xform* xform)
{
	// rotation
	{
		const uint16_t numKeys = curve->numRotKeys;
	
		uint16_t idx = 0;
		
		// this could be a binary search
		while(idx < (numKeys-1) && fa_decompress_key_time(curve->rotKeys[idx].keyTime) < time)
		{
			++idx;
		}
		
		const uint16_t upperIdx = idx;
		const uint16_t lowerIdx = idx == 0 ? idx : idx - 1;
		
		fm_quat rot;
		
		if(lowerIdx == upperIdx)
		{
			fa_decompress_rotation_key(&curve->rotKeys[idx], &rot);
		}
		else
		{
			fm_quat rot1;
			fa_decompress_rotation_key(&curve->rotKeys[lowerIdx], &rot1);
			
			fm_quat rot2;
			fa_decompress_rotation_key(&curve->rotKeys[upperIdx], &rot2);
			
			const float time1 = fa_decompress_key_time(curve->rotKeys[lowerIdx].keyTime);
			const float time2 = fa_decompress_key_time(curve->rotKeys[upperIdx].keyTime);
			
			float alpha = (time - time1) / (time2 - time1);
			fm_quat_lerp(&rot1, &rot2, alpha, &rot);
			fm_quat_norm(&rot);
		}
		
		xform->rot = rot;
	}
	
	// position
	{
		const uint16_t numKeys = curve->numPosKeys;
	
		uint16_t idx = 0;
		
		// this could be a binary search
		while(idx < (numKeys-1) && fa_decompress_key_time(curve->posKeys[idx].keyTime) < time)
		{
			++idx;
		}
		
		const uint16_t upperIdx = idx;
		const uint16_t lowerIdx = idx == 0 ? idx : idx - 1;
		
		fm_vec4 pos;
		
		if(lowerIdx == upperIdx)
		{
			fa_decompress_position_key(&curve->posKeys[idx], &pos);
		}
		else
		{
			fm_vec4 pos1;
			fa_decompress_position_key(&curve->posKeys[lowerIdx], &pos1);
			
			fm_vec4 pos2;
			fa_decompress_position_key(&curve->posKeys[upperIdx], &pos2);
			
			const float time1 = fa_decompress_key_time(curve->posKeys[lowerIdx].keyTime);
			const float time2 = fa_decompress_key_time(curve->posKeys[upperIdx].keyTime);
			
			float alpha = (time - time1) / (time2 - time1);
			fm_vec4_lerp(&pos2, &pos1, alpha, &pos);
		}
		
		xform->pos = pos;
	}
	
	if(asAdditive)
	{
		fm_xform firstKey;
		fa_decompress_rotation_key(&curve->rotKeys[0], &firstKey.rot);
		fa_decompress_position_key(&curve->posKeys[0], &firstKey.pos);
		
		fm_quat_conj(&firstKey.rot);
		
		fm_quat_mul(&firstKey.rot, &xform->rot, &xform->rot);
		fm_vec4_sub(&xform->pos, &firstKey.pos, &xform->pos);
	}
}

void fa_anim_clip_sample(const fa_anim_clip_t* clip, float time, bool asAdditive, fa_pose_t* pose, const uint8_t* mask)
{
	for(uint32_t i=0; i<pose->numXforms; ++i)
	{
		pose->weightsXforms[i] = 0;
	}
	
	for(uint32_t i=0; i<pose->numTracks; ++i)
	{
		pose->weightsTracks[i] = 0;
	}
	
	const uint32_t numCurves = clip->numCurves;
	for(uint32_t i_c=0; i_c<numCurves; ++i_c)
	{
		const fa_anim_curve_t* curve = &clip->curves[i_c];
		const uint16_t idxXform = curve->index;
		
		fa_anim_curve_sample(curve, time, asAdditive, &pose->xforms[idxXform]);
		
		if(mask)
		{
			pose->weightsXforms[idxXform] = mask[idxXform];
		}
		else
		{
			pose->weightsXforms[idxXform] = 255;
		}
	}
}

typedef enum fa_anim_clip_version_t
{
	FA_ANIM_VER_BASE = 0,
	FA_ANIM_VER_LAST,
} fa_anim_clip_version_t;

void fa_anim_clip_serialize(fc_serializer_t* pSerializer, fa_anim_clip_t* clip, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_SER_VERSION(FA_ANIM_VER_LAST-1);
	
	FUR_SER_ADD(FA_ANIM_VER_BASE, clip->name);
	FUR_SER_ADD(FA_ANIM_VER_BASE, clip->duration);
	FUR_SER_ADD(FA_ANIM_VER_BASE, clip->numCurves);
	FUR_SER_ADD(FA_ANIM_VER_BASE, clip->numDataKeys);
	
	if(!pSerializer->isWriting)
	{
		clip->curves = FUR_ALLOC_ARRAY_AND_ZERO(fa_anim_curve_t, clip->numCurves, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		clip->dataKeys = FUR_ALLOC_ARRAY_AND_ZERO(fa_anim_curve_key_t, clip->numDataKeys, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	}
	
	for(int32_t i=0; i<clip->numCurves; ++i)
	{
		FUR_SER_ADD(FA_ANIM_VER_BASE, clip->curves[i]);
	}
	
	FUR_SER_ADD_BUFFER(FA_ANIM_VER_BASE, clip->dataKeys, clip->numDataKeys * sizeof(fa_anim_curve_key_t));
	
	// fix pointers for anim curves
	uint16_t keyCounter = 0;
	for(int32_t i=0; i<clip->numCurves; ++i)
	{
		clip->curves[i].rotKeys = clip->dataKeys + keyCounter;
		keyCounter += clip->curves[i].numRotKeys;
		
		clip->curves[i].posKeys = clip->dataKeys + keyCounter;
		keyCounter += clip->curves[i].numPosKeys;
	}
	
	for(int32_t i=0; i<8; ++i)
	{
		FUR_SER_ADD(FA_ANIM_VER_BASE, clip->motionDelta[i]);
	}
}

void fa_serialize_anim_curve(fc_serializer_t* pSerializer, fa_anim_curve_t* animCurve)
{
	FUR_SER_ADD(FA_ANIM_VER_BASE, animCurve->index);
	FUR_SER_ADD(FA_ANIM_VER_BASE, animCurve->numRotKeys);
	FUR_SER_ADD(FA_ANIM_VER_BASE, animCurve->numPosKeys);
}
