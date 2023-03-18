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

f32 fa_decompress_float_minus_one_plus_one(u16 value)
{
	return (((f32)value) / 65535.0f) * 2.0f - 1.0f;
}

u16 fa_compress_float_minus_one_plus_one(f32 value)
{
	return (u16)(((value + 1.0f) / 2.0f) * 65535.0f);
}

void fm_vec3_to_16bit(const fm_vec3* v, u16* b)
{
	b[0] = fa_compress_float_minus_one_plus_one(v->x);
	b[1] = fa_compress_float_minus_one_plus_one(v->y);
	b[2] = fa_compress_float_minus_one_plus_one(v->z);
}

void fm_16bit_to_vec3(const u16* b, fm_vec3* v)
{
	v->x = fa_decompress_float_minus_one_plus_one(b[0]);
	v->y = fa_decompress_float_minus_one_plus_one(b[1]);
	v->z = fa_decompress_float_minus_one_plus_one(b[2]);
}

const f32 Km  = 4.0*(0.4142135679721832275390625); // 4(sqrt(2)-1)
const f32 Khf = 2.414213657379150390625;           // sqrt(2)+1 = 1/(sqrt(2)-1)
const f32 Khi = 0.17157287895679473876953125;      // 3-2sqrt(2)

void quat_fhm(fm_quat q, fm_vec3* v)
{
	f32 s = Khf / (1.0f + q.r + sqrtf(2.0f + 2.0f * q.r));
	
	v->x = q.i * s;
	v->y = q.j * s;
	v->z = q.k * s;
}

fm_quat quat_ihm(const fm_vec3 v)
{
	f32 d = Khi * fm_vec3_dot(v, v);
	f32 a = (1.0f + d);
	f32 b = (1.0f - d) * Km;
	f32 c = 1.0f / (a * a);
	fm_quat q;
	
	f32 bc = b * c;
	
	q.i = v.x * bc;
	q.j = v.y * bc;
	q.k = v.z * bc;
	q.r = (1.0f + d * (d - 6.0f)) * c;
	
	return q;
}

void quat_fhm_16bit(fm_quat q, u16* v)
{
	fm_vec3 vec;
	quat_fhm(q, &vec);
	fm_vec3_to_16bit(&vec, v);
}

fm_quat quat_ihm_16bit(const u16* b)
{
	fm_vec3 vec;
	fm_16bit_to_vec3(b, &vec);
	return quat_ihm(vec);
}

const f32 c_posRange = 20.0f;

void vec4_com_16bit(fm_vec4 v, u16* b)
{
	fm_vec3 vec = {v.x / c_posRange, v.y / c_posRange, v.z / c_posRange};
	fm_vec3_to_16bit(&vec, b);
}

fm_vec4 vec4_decom_16bit(const u16* v)
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
	const u16* keyData = key->keyData;
	*rot = quat_ihm_16bit(keyData);
}

void fa_decompress_position_key(const fa_anim_curve_key_t* key, fm_vec4* pos)
{
	const u16* keyData = key->keyData;
	*pos = vec4_decom_16bit(keyData);
}

f32 fa_decompress_key_time(const u16 time)
{
	return ((f32)time) / 24.0f;
}

void fa_anim_curve_sample(const fa_anim_curve_t* curve, f32 time, bool asAdditive, fm_xform* xform)
{
	// rotation
	{
		const u16 numKeys = curve->numRotKeys;
	
		u16 idx = 0;
		
		// this could be a binary search
		while(idx < (numKeys-1) && fa_decompress_key_time(curve->rotKeys[idx].keyTime) < time)
		{
			++idx;
		}
		
		const u16 upperIdx = idx;
		const u16 lowerIdx = idx == 0 ? idx : idx - 1;
		
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
			
			const f32 time1 = fa_decompress_key_time(curve->rotKeys[lowerIdx].keyTime);
			const f32 time2 = fa_decompress_key_time(curve->rotKeys[upperIdx].keyTime);
			
			f32 alpha = (time - time1) / (time2 - time1);
			rot = fm_quat_norm(fm_quat_lerp(rot1, rot2, alpha));
		}
		
		xform->rot = rot;
	}
	
	// position
	{
		const u16 numKeys = curve->numPosKeys;
	
		u16 idx = 0;
		
		// this could be a binary search
		while(idx < (numKeys-1) && fa_decompress_key_time(curve->posKeys[idx].keyTime) < time)
		{
			++idx;
		}
		
		const u16 upperIdx = idx;
		const u16 lowerIdx = idx == 0 ? idx : idx - 1;
		
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
			
			const f32 time1 = fa_decompress_key_time(curve->posKeys[lowerIdx].keyTime);
			const f32 time2 = fa_decompress_key_time(curve->posKeys[upperIdx].keyTime);
			
			f32 alpha = (time - time1) / (time2 - time1);
			pos = fm_vec4_lerp(pos2, pos1, alpha);
		}
		
		xform->pos = pos;
	}
	
	if(asAdditive)
	{
		fm_xform firstKey;
		fa_decompress_rotation_key(&curve->rotKeys[0], &firstKey.rot);
		fa_decompress_position_key(&curve->posKeys[0], &firstKey.pos);
		
		firstKey.rot = fm_quat_conj(firstKey.rot);
		
		xform->rot = fm_quat_mul(firstKey.rot, xform->rot);
		xform->pos = fm_vec4_sub(xform->pos, firstKey.pos);
	}
}

void fa_anim_clip_sample(const fa_anim_clip_t* clip, f32 time, bool asAdditive, fa_pose_t* pose, const u8* mask)
{
	FUR_PROFILE("anim-clip-sample")
	{
		for(u32 i=0; i<pose->numXforms; ++i)
		{
			pose->weightsXforms[i] = 0;
		}
		
		for(u32 i=0; i<pose->numTracks; ++i)
		{
			pose->weightsTracks[i] = 0;
		}
		
		const u32 numCurves = clip->numCurves;
		for(u32 i_c=0; i_c<numCurves; ++i_c)
		{
			const fa_anim_curve_t* curve = &clip->curves[i_c];
			const u16 idxXform = curve->index;
			
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
	
	for(i32 i=0; i<clip->numCurves; ++i)
	{
		FUR_SER_ADD(FA_ANIM_VER_BASE, clip->curves[i]);
	}
	
	FUR_SER_ADD_BUFFER(FA_ANIM_VER_BASE, clip->dataKeys, clip->numDataKeys * sizeof(fa_anim_curve_key_t));
	
	// fix pointers for anim curves
	u16 keyCounter = 0;
	for(i32 i=0; i<clip->numCurves; ++i)
	{
		clip->curves[i].rotKeys = clip->dataKeys + keyCounter;
		keyCounter += clip->curves[i].numRotKeys;
		
		clip->curves[i].posKeys = clip->dataKeys + keyCounter;
		keyCounter += clip->curves[i].numPosKeys;
	}
	
	for(i32 i=0; i<8; ++i)
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
