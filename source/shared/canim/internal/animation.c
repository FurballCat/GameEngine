/* Copyright (c) 2016-2020 Furball Cat */

#include "animation.h"
#include "ccore/public.h"
#include "cmath/public.h"
#include <string.h>
#include <stdio.h>

#define MIN(x, y) x < y ? x : y

void fa_rig_release(fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(rig->boneNameHashes, pAllocCallbacks);
	FUR_FREE(rig->parents, pAllocCallbacks);
	FUR_FREE(rig->refPose, pAllocCallbacks);
	
	if(rig->maskUpperBody)
		FUR_FREE(rig->maskUpperBody, pAllocCallbacks);
	if(rig->maskFace)
		FUR_FREE(rig->maskFace, pAllocCallbacks);
	
	FUR_FREE(rig, pAllocCallbacks);
}

int16_t fa_rig_find_bone_idx(const fa_rig_t* rig, fc_string_hash_t name)
{
	for(int32_t i=0; i<rig->numBones; ++i)
	{
		if(rig->boneNameHashes[i] == name)
			return i;
	}
	
	return -1;
}

const uint8_t* fa_rig_get_mask(const fa_rig_t* rig, fa_mask_t mask)
{
	if(mask == FA_MASK_UPPER_BODY)
		return rig->maskUpperBody;
	else if(mask == FA_MASK_FACE)
		return rig->maskFace;
	
	return NULL;
}

void fa_anim_clip_release(fa_anim_clip_t* clip, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(clip->curves, pAllocCallbacks);
	FUR_FREE(clip->dataKeys, pAllocCallbacks);
	
	FUR_FREE(clip, pAllocCallbacks);
}


// -----

void fa_pose_set_identity(fa_pose_t* pose, const uint8_t* mask)
{
	fm_xform* xforms = pose->xforms;
	float* tracks = pose->tracks;
	
	const uint8_t* weightsXformsBegin = pose->weightsXforms;
	const uint8_t* weightsTracksBegin = pose->weightsTracks;
	
	const uint8_t* weightsXforms = weightsXformsBegin;
	const uint8_t* weightsTracks = weightsTracksBegin;
	
	for(uint32_t i=0; i<pose->numXforms; ++i)
	{
		const uint8_t maskValue = mask ? mask[i] : 255;
		const uint8_t weight = weightsXformsBegin ? *weightsXforms : maskValue;
		
		if(weight != 0)
		{
			fm_xform_identity(xforms);
		}
		
		xforms++;
		weightsXforms++;
	}
	
	for(uint32_t i=0; i<pose->numTracks; ++i)
	{
		const uint8_t weight = weightsTracksBegin ? *weightsTracks : 255;
		
		if(weight != 0)
		{
			*tracks = 0.0f;
		}
		
		tracks++;
		weightsTracks++;
	}
}

void fa_pose_set_reference(const fa_rig_t* rig, fa_pose_t* pose, const uint8_t* mask)
{
	FUR_ASSERT(rig->numBones == pose->numXforms);
	
	const fm_xform* refXForms = rig->refPose;
	fm_xform* xforms = pose->xforms;
	float* tracks = pose->tracks;
	
	const uint8_t* weightsXformsBegin = pose->weightsXforms;
	const uint8_t* weightsTracksBegin = pose->weightsTracks;
	
	const uint8_t* weightsXforms = weightsXformsBegin;
	const uint8_t* weightsTracks = weightsTracksBegin;
	
	for(uint32_t i=0; i<pose->numXforms; ++i)
	{
		const uint8_t maskValue = mask ? mask[i] : 255;
		const uint8_t weight = weightsXformsBegin ? *weightsXforms : maskValue;
		
		if(weight != 0)
		{
			*xforms = *refXForms;
		}
		
		xforms++;
		weightsXforms++;
		refXForms++;
	}
	
	for(uint32_t i=0; i<pose->numTracks; ++i)
	{
		const uint8_t weight = weightsTracksBegin ? *weightsTracks : 255;
		
		if(weight != 0)
		{
			*tracks = 0.0f;
		}
		
		tracks++;
		weightsTracks++;
	}
}

// -----

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
			
			pose->xforms[idxXform].rot = rot;
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
			
			pose->xforms[idxXform].pos = pos;
		}
		
		if(mask)
		{
			pose->weightsXforms[idxXform] = mask[idxXform];
		}
		else
		{
			pose->weightsXforms[idxXform] = 255;
		}
		
		if(asAdditive)
		{
			fm_xform firstKey;
			fa_decompress_rotation_key(&curve->rotKeys[0], &firstKey.rot);
			fa_decompress_position_key(&curve->posKeys[0], &firstKey.pos);
			
			fm_quat_conj(&firstKey.rot);
			
			fm_quat_mul(&firstKey.rot, &pose->xforms[idxXform].rot, &pose->xforms[idxXform].rot);
			fm_vec4_sub(&pose->xforms[idxXform].pos, &firstKey.pos, &pose->xforms[idxXform].pos);
		}
	}
}

// -----

void fa_pose_copy(fa_pose_t* dest, const fa_pose_t* src)
{
	const uint32_t numXforms = MIN(src->numXforms, dest->numXforms);
	if(numXforms > 0)
	{
		memcpy(dest->xforms, src->xforms, sizeof(fm_xform) * numXforms);
		if(dest->weightsXforms && src->weightsXforms)
		{
			memcpy(dest->weightsXforms, src->weightsXforms, sizeof(uint8_t) * numXforms);
		}
	}
	
	const uint32_t numTracks = MIN(src->numTracks, dest->numTracks);
	if(numTracks > 0)
	{
		memcpy(dest->tracks, src->tracks, sizeof(float) * numTracks);
		if(dest->weightsTracks && src->weightsTracks)
		{
			memcpy(dest->weightsTracks, src->weightsTracks, sizeof(uint8_t) * numXforms);
		}
	}
}

void fa_pose_local_to_model(fa_pose_t* modelPose, const fa_pose_t* localPose, const int16_t* parentIndices)
{
	const fm_xform* localXforms = localPose->xforms;
	fm_xform* modelXforms = modelPose->xforms;
	
	uint32_t numBones = MIN(modelPose->numXforms, localPose->numXforms);
	
	// iterate non-root bones
	for(uint16_t i = 0; i < numBones; ++i)
	{
		const int16_t idxParent = parentIndices[i];
		if(idxParent >= 0)
		{
			fm_xform_mul(&modelXforms[idxParent], &localXforms[i], &modelXforms[i]);
		}
		else
		{
			modelXforms[i] = localXforms[i];
		}
	}
	
	// copy tracks
	uint32_t numTracks = MIN(modelPose->numTracks, localPose->numTracks);
	if(numTracks > 0)
	{
		memcpy(modelPose->tracks, localPose->tracks, sizeof(float) * numTracks);
	}
}

void fa_pose_blend_linear(fa_pose_t* out, const fa_pose_t* b, const fa_pose_t* a, float alpha)
{
	FUR_ASSERT(out->numXforms == a->numXforms && a->numXforms == b->numXforms);
	FUR_ASSERT(out->numTracks == a->numTracks && a->numTracks == b->numTracks);
	FUR_ASSERT((out->weightsXforms || out->numXforms == 0) && (out->weightsTracks || out->numTracks == 0));
	
	// blend xforms
	{
		const uint32_t numXforms = out->numXforms;
		const fm_xform* a_xforms = a->xforms;
		const fm_xform* b_xforms = b->xforms;
		fm_xform* out_xforms = out->xforms;

		const uint8_t* const a_weightsBase = a->weightsXforms;
		const uint8_t* const b_weightsBase = b->weightsXforms;
		
		const uint8_t* a_weights = a_weightsBase;
		const uint8_t* b_weights = b_weightsBase;
		uint8_t* out_weights = out->weightsXforms;

		for(uint32_t i=0; i<numXforms; ++i)
		{
			const uint8_t a_byte = a_weightsBase ? *a_weights : 255;
			const uint8_t b_byte = b_weightsBase ? *b_weights : 255;
			
			const bool a_valid = (a_byte != 0);
			const bool b_valid = (b_byte != 0);
			
			if(a_valid && b_valid)
			{
				const float a_weight = a_byte * (1.0f / 255.0f);
				const float b_weight = b_byte * (1.0f / 255.0f);
				
				const float blendFactor = (b_weight > a_weight) ? ((b_weight - a_weight + alpha * a_weight) / b_weight) : alpha * b_weight / a_weight;
				const float outWeight = (1.0f - blendFactor) * a_weight + blendFactor * b_weight;
				
				fm_xform_slerp(a_xforms, b_xforms, blendFactor, out_xforms);
				
				*out_weights = (uint8_t)(outWeight * 255.0f + 0.5f);
			}
			else if(a_valid)
			{
				*out_xforms = *a_xforms;
				*out_weights = *a_weights;
			}
			else if(b_valid)
			{
				*out_xforms = *b_xforms;
				*out_weights = *b_weights;
			}
			else
			{
				*out_weights = 0x00;
			}
			
			a_xforms++;
			b_xforms++;
			out_xforms++;
			a_weights++;
			b_weights++;
			out_weights++;
		}
	}
	
	// blend tracks
	{
		const uint32_t numTracks = out->numTracks;
		const float* a_tracks = a->tracks;
		const float* b_tracks = b->tracks;
		float* out_tracks = out->tracks;
		
		const uint8_t* const a_weightsBase = a->weightsTracks;
		const uint8_t* const b_weightsBase = b->weightsTracks;
		
		const uint8_t* a_weights = a_weightsBase;
		const uint8_t* b_weights = b_weightsBase;
		uint8_t* out_weights = out->weightsTracks;
		
		for(uint32_t i=0; i<numTracks; ++i)
		{
			const uint8_t a_byte = a_weightsBase ? *a_weights : 255;
			const uint8_t b_byte = b_weightsBase ? *b_weights : 255;
			
			const bool a_valid = (a_byte != 0);
			const bool b_valid = (b_byte != 0);
			
			if(a_valid && b_valid)
			{
				const float a_weight = a_byte * (1.0f / 255.0f);
				const float b_weight = b_byte * (1.0f / 255.0f);
				
				const float blendFactor = (b_weight > a_weight) ? ((b_weight - a_weight + alpha * a_weight) / b_weight) : alpha * b_weight / a_weight;
				const float outWeight = (1.0f - blendFactor) * a_weight + blendFactor * b_weight;
				
				*out_tracks = (1.0f - blendFactor) * (*a_tracks) + blendFactor * (*b_tracks);
				
				*out_weights = (uint8_t)(outWeight * 255.0f + 0.5f);
			}
			else if(a_valid)
			{
				*out_tracks = *a_tracks;
				*out_weights = *a_weights;
			}
			else if(b_valid)
			{
				*out_tracks = *b_tracks;
				*out_weights = *b_weights;
			}
			else
			{
				*out_weights = 0x00;
			}
			
			a_tracks++;
			b_tracks++;
			out_tracks++;
			a_weights++;
			b_weights++;
			out_weights++;
		}
	}
}

void fa_pose_apply_additive(fa_pose_t* out, const fa_pose_t* base, const fa_pose_t* add, float weight)
{
	// blend xforms
	{
		const uint32_t numXforms = out->numXforms;
		const fm_xform* base_xforms = base->xforms;
		const fm_xform* add_xforms = add->xforms;
		fm_xform* out_xforms = out->xforms;
		
		if(weight == 1.0f)
		{
			for(uint32_t i=0; i<numXforms; ++i)
			{
				fm_xform_mul(&base_xforms[i], &add_xforms[i], &out_xforms[i]);
			}
		}
		else
		{
			for(uint32_t i=0; i<numXforms; ++i)
			{
				fm_xform addXform = add_xforms[i];
				fm_vec4_mulf(&addXform.pos, weight, &addXform.pos);
				fm_quat identity;
				fm_quat_identity(&identity);
				fm_quat_slerp(&identity, &addXform.rot, weight, &addXform.rot);
				fm_xform_mul(&base_xforms[i], &addXform, &out_xforms[i]);
			}
		}
	}
	
	// blend tracks
	{
		const uint32_t numTracks = out->numTracks;
		const float* base_tracks = base->tracks;
		const float* add_tracks = add->tracks;
		float* out_tracks = out->tracks;
		
		for(uint32_t i=0; i<numTracks; ++i)
		{
			out_tracks[i] = base_tracks[i] + add_tracks[i] * weight;
		}
	}
}

CANIM_API void fa_pose_stack_init(fa_pose_stack_t* pStack, const fa_pose_stack_desc_t* desc, void* buffer, uint32_t bufferSize)
{
	FUR_ASSERT(pStack);
	FUR_ASSERT(pStack->buffer == NULL);
	
	const uint32_t sizeXforms = desc->numBonesPerPose * sizeof(fm_xform);
	const uint32_t sizeWeightXforms = desc->numBonesPerPose * sizeof(uint8_t);
	const uint32_t sizeTracks = desc->numTracksPerPose * sizeof(float);
	const uint32_t sizeWeightTracks = desc->numTracksPerPose * sizeof(uint8_t);
	
	uint32_t poseSize = sizeXforms + sizeTracks + sizeWeightXforms + sizeWeightTracks;
	poseSize += 16 - poseSize % 16;	// align pose size to 16 (add padding, so the next pose will be aligned to 16)
	uint32_t bufferSizeRequired = poseSize * desc->numMaxPoses;
	
	FUR_ASSERT(bufferSize >= bufferSizeRequired);
	
	pStack->buffer = buffer;
	pStack->bufferSize = bufferSizeRequired;
	pStack->numBones = desc->numBonesPerPose;
	pStack->numTracks = desc->numTracksPerPose;
	pStack->numMaxPoses = desc->numMaxPoses;
	pStack->numPoses = 0;
	pStack->poseSize = poseSize;
	pStack->offsetTracks = sizeXforms;
	pStack->offsetWeightXforms = sizeXforms + sizeTracks;
	pStack->offsetWeightTracks = sizeXforms + sizeTracks + sizeWeightXforms;
}

CANIM_API void fa_pose_stack_release(fa_pose_stack_t* pStack)
{
	FUR_ASSERT(pStack->buffer != NULL);
	
	memset(pStack, 0, sizeof(fa_pose_stack_t));
}

CANIM_API void fa_pose_stack_push(fa_pose_stack_t* pStack, uint32_t count)
{
	FUR_ASSERT(pStack->buffer != NULL);
	FUR_ASSERT(pStack->numPoses + count <= pStack->numMaxPoses);
	
	pStack->numPoses += count;
}

CANIM_API void fa_pose_stack_pop(fa_pose_stack_t* pStack, uint32_t count)
{
	FUR_ASSERT(pStack->buffer != NULL);
	FUR_ASSERT(pStack->numPoses >= count);
	
	pStack->numPoses -= count;
}

CANIM_API void fa_pose_stack_get(const fa_pose_stack_t* pStack, fa_pose_t* pPose, uint32_t depth)
{
	FUR_ASSERT(pStack->buffer != NULL);
	FUR_ASSERT(pStack->numPoses > depth);
	
	const uint32_t poseIndex = pStack->numPoses - 1 - depth;
	const uint32_t poseOffset = poseIndex * pStack->poseSize;
	void* poseData = pStack->buffer + poseOffset;
	
	pPose->numXforms = pStack->numBones;
	pPose->numTracks = pStack->numTracks;
	pPose->flags = 0;	// todo: we should store flags somewhere
	
	if(pStack->numBones > 0)
	{
		pPose->xforms = (fm_xform*)poseData;
		pPose->weightsXforms = (uint8_t*)(poseData + pStack->offsetWeightXforms);
	}
	else
	{
		pPose->xforms = NULL;
		pPose->weightsXforms = NULL;
	}
	
	if(pStack->numTracks > 0)
	{
		pPose->tracks = (float*)(poseData + pStack->offsetTracks);
		pPose->weightsTracks = (uint8_t*)(poseData + pStack->offsetWeightTracks);
	}
	else
	{
		pPose->tracks = NULL;
		pPose->weightsTracks = NULL;
	}
	
}

// ******************* COMMANDS ******************* //

void fa_cmd_buffer_evaluate(const fa_cmd_buffer_t* buffer, fa_cmd_context_t* ctx)
{
	fa_cmd_status_t status = FA_CMD_STATUS_OK;
	uint32_t dbgIndexCommand = 0;
	const void* cmdPointer = buffer->data;
	while(status == FA_CMD_STATUS_OK)
	{
		const fa_cmd_func_t* func = (const fa_cmd_func_t*)cmdPointer;
		const uint32_t* dataSize = (const uint32_t*)(cmdPointer + sizeof(fa_cmd_func_t));
		const void* cmdData = NULL;
		if(*dataSize > 0)
		{
			 cmdData = cmdPointer + sizeof(fa_cmd_func_t) + sizeof(uint32_t);
		}
		
		status = (*func)(ctx, cmdData);
		const uint32_t totalCommandSize = sizeof(fa_cmd_func_t) + sizeof(uint32_t) + (*dataSize);
		cmdPointer += totalCommandSize;
		++dbgIndexCommand;
		
		if(ctx->debug)
		{
			ctx->debug->cmdDrawCursorVerticalPos += 14;
		}
	}
}

// this is the cmd memory formatting function
void fa_cmd_buffer_write(fa_cmd_buffer_recorder_t* recorder, fa_cmd_func_t func, const void* data, uint32_t dataSize)
{
	const uint32_t sizeRequired = sizeof(fa_cmd_func_t) + sizeof(uint32_t) + dataSize;
	FUR_ASSERT(sizeRequired <= recorder->sizeLeft);

	// command function pointer
	fa_cmd_func_t* funcPtr = (fa_cmd_func_t*)recorder->currPointer;
	*funcPtr = func;
	
	// command data size (can be 0)
	uint32_t* dataSizePtr = (uint32_t*)(recorder->currPointer + sizeof(fa_cmd_func_t));
	*dataSizePtr = dataSize;
	
	// command data
	if(data)
	{
		void* dataPtr = recorder->currPointer + sizeof(fa_cmd_func_t) + sizeof(uint32_t);
		memcpy(dataPtr, data, dataSize);
	}
	
	recorder->currPointer += sizeRequired;
	recorder->sizeLeft -= sizeRequired;
	recorder->sizeRecorded += sizeRequired;
}

void fa_cmd_buffer_recorder_init(fa_cmd_buffer_recorder_t* recorder, void* outData, uint32_t maxSize)
{
	recorder->currPointer = outData;
	recorder->sizeLeft = maxSize;
	recorder->sizeRecorded = 0;
	recorder->poseStackSizeTracking = 0;
}

// begin command
void fa_cmd_begin(fa_cmd_buffer_recorder_t* recorder, uint32_t poseStackInitialSize)
{
	recorder->poseStackSizeTracking = poseStackInitialSize;
	recorder->poseStackInitialSize = poseStackInitialSize;
}

// end command
fa_cmd_status_t fa_cmd_impl_end(fa_cmd_context_t* ctx, const void* cmdData)
{
	return FA_CMD_STATUS_STOP;
}

void fa_cmd_end(fa_cmd_buffer_recorder_t* recorder)
{
	fa_cmd_buffer_write(recorder, fa_cmd_impl_end, NULL, 0); // writing a null cmd, similar to c-string having null character at the end
	
	FUR_ASSERT(recorder->poseStackSizeTracking >= 1);	// at the end of command buffer, we expect the post stack to have +1 pose
}

#define FA_DBG_TEXT_X -900.0f
#define FA_DBG_TEXT_Y(_pos) 150.0f - 2.0f * _pos
#define FA_DBG_COLOR FUR_COLOR_GREEN

// set reference pose command
fa_cmd_status_t fa_cmd_impl_ref_pose(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_pose_stack_push(ctx->poseStack, 1);
	
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	fa_pose_set_reference(ctx->rig, &pose, ctx->mask);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "ref_pose", color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_ref_pose(fa_cmd_buffer_recorder_t* recorder)
{
	fa_cmd_buffer_write(recorder, fa_cmd_impl_ref_pose, NULL, 0);
	recorder->poseStackSizeTracking += 1;
}

// set identity pose command
fa_cmd_status_t fa_cmd_impl_identity(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_pose_stack_push(ctx->poseStack, 1);
	
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	fa_pose_set_identity(&pose, ctx->mask);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "identity", color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_identity(fa_cmd_buffer_recorder_t* recorder)
{
	fa_cmd_buffer_write(recorder, fa_cmd_impl_identity, NULL, 0);
	recorder->poseStackSizeTracking += 1;
}

// sample animation command
typedef struct fa_cmd_anim_sample_data_t
{
	float time;
	uint16_t animClipId;
	bool asAdditive;
} fa_cmd_anim_sample_data_t;

fa_cmd_status_t fa_cmd_impl_anim_sample(fa_cmd_context_t* ctx, const void* cmdData)
{
	const fa_cmd_anim_sample_data_t* data = (fa_cmd_anim_sample_data_t*)cmdData;
	FUR_ASSERT(data->animClipId < ctx->numAnimClips);
	
	const fa_anim_clip_t* clip = ctx->animClips[data->animClipId];
	
	fa_pose_stack_push(ctx->poseStack, 1);
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	FUR_ASSERT(pose.numXforms == ctx->rig->numBones);
	
	fa_anim_clip_sample(clip, data->time, data->asAdditive, &pose, ctx->mask);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[256];
		sprintf(txt, "anim_sample %s t=%1.2f", fc_string_hash_as_cstr_debug(clip->name), data->time);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_anim_sample(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId)
{
	fa_cmd_anim_sample_data_t data = { time, animClipId, false };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_anim_sample, &data, sizeof(fa_cmd_anim_sample_data_t));
	recorder->poseStackSizeTracking += 1;
}

void fa_cmd_anim_sample_additive(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId)
{
	fa_cmd_anim_sample_data_t data = { time, animClipId, true };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_anim_sample, &data, sizeof(fa_cmd_anim_sample_data_t));
	recorder->poseStackSizeTracking += 1;
}

// blend two poses command
typedef struct fa_cmd_blend2_data_t
{
	float alpha;
} fa_cmd_blend2_data_t;

fa_cmd_status_t fa_cmd_impl_blend2(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_cmd_blend2_data_t* data = (fa_cmd_blend2_data_t*)cmdData;
	
	fa_pose_t pose1;
	fa_pose_stack_get(ctx->poseStack, &pose1, 0);
	
	fa_pose_t pose2;
	fa_pose_stack_get(ctx->poseStack, &pose2, 1);
	
	fa_pose_blend_linear(&pose2, &pose1, &pose2, data->alpha);
	
	fa_pose_stack_pop(ctx->poseStack, 1);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "blend2 a=%1.2f", data->alpha);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_blend2(fa_cmd_buffer_recorder_t* recorder, float alpha)
{
	fa_cmd_blend2_data_t data = { alpha };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_blend2, &data, sizeof(fa_cmd_blend2_data_t));
	recorder->poseStackSizeTracking -= 1;
}

// blend additive command, pose depth-0 is additive, pose depth-1 is base
typedef struct fa_cmd_apply_additive_data_t
{
	float weight;
} fa_cmd_apply_additive_data_t;

fa_cmd_status_t fa_cmd_impl_apply_additive(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_cmd_apply_additive_data_t* data = (fa_cmd_apply_additive_data_t*)cmdData;
	
	fa_pose_t poseAdd;
	fa_pose_stack_get(ctx->poseStack, &poseAdd, 0);
	
	fa_pose_t poseBase;
	fa_pose_stack_get(ctx->poseStack, &poseBase, 1);
	
	fa_pose_apply_additive(&poseBase, &poseBase, &poseAdd, data->weight);
	
	fa_pose_stack_pop(ctx->poseStack, 1);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "apply additive w=%1.2f", data->weight);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_apply_additive(fa_cmd_buffer_recorder_t* recorder, float weight)
{
	fa_cmd_apply_additive_data_t data = { weight };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_apply_additive, &data, sizeof(fa_cmd_apply_additive_data_t));
	recorder->poseStackSizeTracking -= 1;
}

// use cached pose command
typedef struct fa_cmd_use_cached_pose_data_t
{
	uint16_t poseId;
} fa_cmd_use_cached_pose_data_t;

fa_cmd_status_t fa_cmd_impl_use_cached_pose(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_cmd_use_cached_pose_data_t* data = (fa_cmd_use_cached_pose_data_t*)cmdData;
	
	FUR_ASSERT(data->poseId == 0);
	
	fa_pose_stack_push(ctx->poseStack, 1);
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	fa_pose_copy(&pose, &ctx->poseCache->tempPose);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "use_cached_pose id=%i", data->poseId);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_use_cached_pose(fa_cmd_buffer_recorder_t* recorder, uint16_t poseId)
{
	fa_cmd_use_cached_pose_data_t data = { poseId };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_use_cached_pose, &data, sizeof(fa_cmd_use_cached_pose_data_t));
	recorder->poseStackSizeTracking += 1;
}

// apply mask command
typedef struct fa_cmd_apply_mask_data_t
{
	uint16_t maskId;
} fa_cmd_apply_mask_data_t;

fa_cmd_status_t fa_cmd_impl_apply_mask(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_cmd_apply_mask_data_t* data = (fa_cmd_apply_mask_data_t*)cmdData;
	const uint8_t* mask = fa_rig_get_mask(ctx->rig, data->maskId);
	if(!mask)
	{
		if(ctx->debug)
		{
			const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
			const float color[4] = FUR_COLOR_RED;
			fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "apply_mask id=<INNVALID>", color);
		}
		
		return FA_CMD_STATUS_OK;
	}
	
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	for(uint32_t i=0; i<pose.numXforms; ++i)
	{
		const uint16_t weight = ((uint16_t)pose.weightsXforms[i] * (uint16_t)mask[i]) / 255;
		pose.weightsXforms[i] = (uint8_t)weight;
	}
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "apply_mask id=%i", data->maskId);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_apply_mask(fa_cmd_buffer_recorder_t* recorder, uint16_t maskId)
{
	fa_cmd_apply_mask_data_t data = { maskId };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_apply_mask, &data, sizeof(fa_cmd_apply_mask_data_t));
}

void fa_action_reset(fa_action_t* action)
{
	memset(action, 0, sizeof(fa_action_t));
}

void fa_character_leg_ik(fa_character_t* character, const fa_ik_setup_t* ikSetup, fa_pose_t* poseLS, fa_pose_t* poseMS, const fm_vec4* targetArg, float weightIK)
{
	fm_vec4 targetFixed = *targetArg;
	targetFixed.z += 0.1f;
	
	fm_xform chainLS[4] = {
		poseLS->xforms[ikSetup->idxBeginParent],
		poseLS->xforms[ikSetup->idxBegin],
		poseLS->xforms[ikSetup->idxMid],
		poseLS->xforms[ikSetup->idxEnd],
	};
	fm_xform chainMS[4] = {
		poseMS->xforms[ikSetup->idxBeginParent],
		poseMS->xforms[ikSetup->idxBegin],
		poseMS->xforms[ikSetup->idxMid],
		poseMS->xforms[ikSetup->idxEnd],
	};
	
	fm_quat originalFootMS = chainMS[3].rot;
	
	const float angleMin = ikSetup->minAngle;
	const float angleMax = ikSetup->maxAngle;
	fm_axis_t hingeAxis = ikSetup->hingeAxisMid;
	
	fm_vec4 endEffector = chainMS[3].pos;
	fm_vec4 target;
	fm_vec4_lerp(&targetFixed, &endEffector, weightIK, &target);
	
	static uint32_t num_iterations = 20;
	for(uint32_t it=0; it<num_iterations; ++it)
	{
		// loop bones in IK setup
		for(uint32_t i=2; i>=1; --i)
		{
			const uint32_t ip = i-1;
			
			endEffector = chainMS[3].pos;
			
			fm_vec4 e_i;
			fm_vec4_sub(&endEffector, &chainMS[i].pos, &e_i);
			fm_vec4 t_i;
			fm_vec4_sub(&target, &chainMS[i].pos, &t_i);
			
			fm_vec4_normalize(&e_i);
			fm_vec4_normalize(&t_i);
			const float angle = -acosf(fm_vec4_dot(&e_i, &t_i));
			const bool canRot = fabsf(angle) > 0.0001f;
			if(canRot)
			{
				fm_vec4 axis;
				fm_vec4_cross(&e_i, &t_i, &axis);
				if(fm_vec4_mag2(&axis) > 0.0f)
				{
					fm_vec4_normalize(&axis);
					
					fm_quat rot;
					fm_quat_rot_axis_angle(&axis, angle, &rot);
					
					fm_quat invMS = chainMS[ip].rot;
					fm_quat_conj(&invMS);
					
					// take axis for hinge
					fm_vec4 jointAxis;
					fm_axis_to_vec4(hingeAxis, &jointAxis);
					fm_quat_rot(&chainMS[i].rot, &jointAxis, &jointAxis);
					
					// rotate
					fm_quat_mul(&rot, &chainMS[i].rot, &chainMS[i].rot);
					
					// hinge constraint
					if(i == 2)
					{
						fm_vec4 jointAxisNew;
						fm_axis_to_vec4(hingeAxis, &jointAxisNew);
						fm_quat_rot(&chainMS[i].rot, &jointAxisNew, &jointAxisNew);
						fm_quat backRot;
						fm_vec4_rot_between(&jointAxisNew, &jointAxis, &backRot);
						fm_quat_mul(&backRot, &chainMS[i].rot, &chainMS[i].rot);
					}
					
					// write back to LS
					fm_quat_mul(&invMS, &chainMS[i].rot, &chainLS[i].rot);
					fm_quat_norm(&chainLS[i].rot);
					
					// constrain angle
					if(i == 2)
					{
						fm_vec4 rotAxis;
						float rotAngle;
						fm_quat_to_axis_angle(&chainLS[i].rot, &rotAxis, &rotAngle);
						
						if(fabsf(rotAngle) > 0.00001f)
						{
							if(rotAngle > FM_PI)
							{
								rotAngle -= 2 * FM_PI;
							}
							
							fm_vec4 origRotAxis;
							fm_axis_to_vec4(hingeAxis, &origRotAxis);
							if(fm_vec4_dot(&origRotAxis, &rotAxis) > 0.0f)
								rotAngle = fm_clamp(rotAngle, angleMin, angleMax);
							else
								rotAngle = fm_clamp(rotAngle, -angleMax, -angleMin);
							
							fm_quat_rot_axis_angle(&rotAxis, rotAngle, &chainLS[i].rot);
							fm_xform_mul(&chainMS[ip], &chainLS[i], &chainMS[i]);
						}
					}
					
					// update children
					for(uint32_t g=i; g<3; ++g)
					{
						fm_xform_mul(&chainMS[g], &chainLS[g+1], &chainMS[g+1]);
					}
				}
			}
			
		}
	}
	
	// recreate original foot MS orientation
	fm_quat invCurrKneeMS = chainMS[2].rot;
	fm_quat_conj(&invCurrKneeMS);
	fm_quat footCorrection;
	fm_quat_mul(&invCurrKneeMS, &originalFootMS, &footCorrection);
	chainLS[3].rot = footCorrection;
	
	// write results to poseLS
	poseLS->xforms[ikSetup->idxBegin] = chainLS[1];
	poseLS->xforms[ikSetup->idxMid] = chainLS[2];
	poseLS->xforms[ikSetup->idxEnd] = chainLS[3];
}

float fa_action_get_local_time(const fa_action_t* action, const fa_character_t* character)
{
	float localTime = -1.0f;
	
	if(character->globalTime >= action->globalStartTime)
	{
		localTime = (float)((character->globalTime - action->globalStartTime) / 1000000.0);
	}
	FUR_ASSERT(localTime != -1.0f);
	
	return localTime;
}

float fa_action_get_alpha(fa_character_t* character, const fa_action_t* action)
{
	if(!action->isUsed)
		return 0.0f;
	
	float alpha = 1.0f;
	
	const fa_action_args_t* args = &action->args;
	const float localTime = fa_action_get_local_time(action, character);
	
	if(args->fadeInSec > 0.0f)
	{
		alpha = fm_clamp(localTime / args->fadeInSec, 0.0f, 1.0f);
		if(args->fadeInCurve == FA_CURVE_UNIFORM_S)
		{
			alpha = fm_curve_uniform_s(alpha);
		}
	}
	
	return alpha;
}

typedef struct fa_cross_layer_context_t
{
	fa_pose_stack_t* poseStack;
	
	void* scratchMemory;
	uint32_t scratchMemorySize;
	
	fa_cmd_context_debug_t* debug;
	
	float dt;
	
	float outWeightLegsIK;
	
	float rootMotionDeltaX;
	float rootMotionDeltaY;
	float rootMotionDeltaYaw;
	
	fm_vec3 lookAtLocator;
	float lookAtWeight;
} fa_cross_layer_context_t;

void fa_character_layer_cache_pose(fa_layer_t* layer, fa_cross_layer_context_t* ctx, float alpha)
{
	fa_pose_t outPose;
	fa_pose_stack_get(ctx->poseStack, &outPose, 0);
	
	fa_pose_copy(&layer->poseCache.tempPose, &outPose);
	layer->transitionPoseCached = true;
	layer->poseCache.alpha = alpha;
	
	if(ctx->debug)
	{
		const float color[4] = FUR_COLOR_RED;
		fc_dbg_text(-450.0f, 1.0f, "caching_pose", color);
	}
}

void fa_character_action_animate(fa_character_t* character, fa_layer_t* layer, fa_action_t* action, fa_cross_layer_context_t* ctx)
{
	FUR_ASSERT(action->fnUpdate != NULL);
	
	fa_cmd_buffer_t animCmdBuffer = { ctx->scratchMemory, ctx->scratchMemorySize };
	fa_cmd_buffer_recorder_t recorder = {};
	fa_cmd_buffer_recorder_init(&recorder, animCmdBuffer.data, animCmdBuffer.size);
	
	// record commands
	FUR_ASSERT(action->fnUpdate != NULL);
	FUR_ASSERT(action->fnGetAnims != NULL);
	
	const float localTime = fa_action_get_local_time(action, character);
	
	fa_action_ctx_t actionCtx = {};
	actionCtx.dt = ctx->dt;
	actionCtx.cmdRecorder = &recorder;
	actionCtx.animInfo = &character->animInfo;
	actionCtx.localTime = localTime;
	actionCtx.debug = ctx->debug;
	
	// record commands
	{
		fa_cmd_begin(&recorder, ctx->poseStack->numPoses);
		action->fnUpdate(&actionCtx, action->userData);
		fa_cmd_end(&recorder);
	}
	
	// evaluate commands
	fa_cmd_context_t animCtx = {};
	animCtx.animClips = action->fnGetAnims(action->userData, &animCtx.numAnimClips);
	animCtx.rig = character->rig;
	animCtx.poseStack = ctx->poseStack;
	animCtx.poseCache = &layer->poseCache;
	animCtx.debug = ctx->debug;
	animCtx.mask = fa_rig_get_mask(character->rig, layer->maskID);
	
	fa_cmd_buffer_evaluate(&animCmdBuffer, &animCtx);
	
	ctx->rootMotionDeltaX = actionCtx.rootMotionDeltaX;
	ctx->rootMotionDeltaY = actionCtx.rootMotionDeltaY;
	ctx->rootMotionDeltaYaw = actionCtx.rootMotionDeltaYaw;
}

void fa_action_safely_cancel(fa_action_t* action)
{
	if(action->isUsed && action->fnCancel)
	{
		action->fnCancel(action->userData);
	}
}

fa_action_t* fa_action_queue_get_current(fa_action_queue_t* queue)
{
	if(queue->actions[0].isUsed)
	{
		return &queue->actions[0];
	}
	
	return NULL;
}

fa_action_t* fa_action_queue_get_next(fa_action_queue_t* queue)
{
	if(queue->actions[1].isUsed)
	{
		return &queue->actions[1];
	}
	
	return NULL;
}

fa_action_t* fa_action_queue_get_free_slot(fa_action_queue_t* queue)
{
	// find free slot
	for(uint32_t i=0; i<4; ++i)
	{
		if(queue->actions[i].isUsed == false)
		{
			return &queue->actions[i];
		}
	}
	
	// if not found, make some slot free
	fa_action_safely_cancel(&queue->actions[2]); // cancel action [2]
	queue->actions[2] = queue->actions[3];	// move action [3] to [2]
	fa_action_reset(&queue->actions[3]);	// clear unused slot
	
	return &queue->actions[3];	// return free, unused slot
}

void fa_action_safely_begin(fa_action_begin_end_ctx_t* ctx, fa_action_t* action)
{
	if(action->isUsed && action->fnBegin)
	{
		action->fnBegin(ctx, action->userData);
	}
}

void fa_action_safely_end(fa_action_begin_end_ctx_t* ctx, fa_action_t* action)
{
	if(action->isUsed && action->fnEnd)
	{
		action->fnEnd(ctx, action->userData);
	}
}

void fa_action_queue_resolve_pre_animate(fa_character_t* character, fa_action_queue_t* queue)
{
	fa_action_begin_end_ctx_t ctx = {};
	ctx.animInfo = &character->animInfo;
	
	// check if any of the pending actions should be instantly activated
	if(fa_action_get_alpha(character, &queue->actions[3]) >= 1.0)
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		fa_action_safely_end(&ctx, &queue->actions[1]);
		
		// rare case when we need to cancel action [2], as it's eaten up by action [3]
		fa_action_safely_cancel(&queue->actions[2]);
		
		// begin new actions
		fa_action_safely_begin(&ctx, &queue->actions[3]);
		
		// move pending actions
		queue->actions[0] = queue->actions[3];
		
		// clear unused slots
		fa_action_reset(&queue->actions[1]);
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
	}
	else if(fa_action_get_alpha(character, &queue->actions[2]) >= 1.0)
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		fa_action_safely_end(&ctx, &queue->actions[1]);
		
		// begin new actions
		fa_action_safely_begin(&ctx, &queue->actions[2]);
		fa_action_safely_begin(&ctx, &queue->actions[3]);
		
		// move pending actions
		queue->actions[0] = queue->actions[2];
		queue->actions[1] = queue->actions[3];
		
		// clear unused slots
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
	}
	else if(queue->actions[2].isUsed && queue->actions[3].isUsed)
	{
		queue->cachePoseAfterNextAction = true;
	}
	else if(queue->actions[2].isUsed)
	{
		queue->cachePoseAfterCurrAction = true;
	}
}

void fa_action_queue_resolve_post_animate(fa_character_t* character, fa_action_queue_t* queue)
{
	fa_action_begin_end_ctx_t ctx = {};
	ctx.animInfo = &character->animInfo;
	
	if(queue->cachePoseAfterNextAction) // case of cache for 2 actions
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		fa_action_safely_end(&ctx, &queue->actions[1]);
		
		// begin new actions
		fa_action_safely_begin(&ctx, &queue->actions[2]);
		fa_action_safely_begin(&ctx, &queue->actions[3]);
		
		// move pending actions
		queue->actions[0] = queue->actions[2];
		queue->actions[1] = queue->actions[3];
		
		// clear unused slots
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
		
		queue->cachePoseAfterNextAction = false;
	}
	else if(queue->cachePoseAfterCurrAction)	// case of cache for 1 action
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		
		// begin new actions
		fa_action_safely_begin(&ctx, &queue->actions[2]);
		
		// move pending actions
		queue->actions[0] = queue->actions[1];
		queue->actions[1] = queue->actions[2];
		
		// clear unused slots
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
		
		queue->cachePoseAfterCurrAction = false;
	}
	else if(fa_action_get_alpha(character, &queue->actions[1]) >= 1.0)	// normal case, just next action
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		
		// begin new actions
		// 'next' action [1] was already begun
		fa_action_safely_begin(&ctx, &queue->actions[2]);
		
		// move actions
		queue->actions[0] = queue->actions[1];
		queue->actions[1] = queue->actions[2];
		queue->actions[2] = queue->actions[3];
		
		// clear unused slots
		fa_action_reset(&queue->actions[1]);
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
	}
}

void fa_character_layer_animate(fa_character_t* character, fa_cross_layer_context_t* ctx, fa_layer_t* layer)
{
	// resolve scheduled actions
	fa_action_queue_resolve_pre_animate(character, &layer->actionQueue);
	
	fa_action_t* currAction = &layer->actionQueue.actions[0];
	fa_action_t* nextAction = &layer->actionQueue.actions[1];
	
	const float nextAlpha = fa_action_get_alpha(character, nextAction);
	const float currAlpha = fa_action_get_alpha(character, currAction) * (1.0f - nextAlpha);
	const float cachedPoseAlpha = layer->poseCache.alpha * (1.0f - currAlpha);
	
	// copy cached pose if required
	if(layer->transitionPoseCached && cachedPoseAlpha > 0.0f)
	{
		fa_pose_t outPose;
		fa_pose_stack_get(ctx->poseStack, &outPose, 0);
		fa_pose_blend_linear(&outPose, &layer->poseCache.tempPose, &outPose, cachedPoseAlpha);		// blend to include pose weights/mask
	}
	else
	{
		layer->transitionPoseCached = false;
	}
	
	// animate current action
	if(currAction->fnUpdate != NULL)
	{
		fa_action_t* action = currAction;
		fa_character_action_animate(character, layer, action, ctx);
		
		if(layer->actionQueue.cachePoseAfterCurrAction)
		{
			fa_character_layer_cache_pose(layer, ctx, currAlpha);
		}
		
		// blend in result
		{
			fa_pose_t actionPose;
			fa_pose_t outPose;
			fa_pose_stack_get(ctx->poseStack, &actionPose, 0);
			fa_pose_stack_get(ctx->poseStack, &outPose, 1);
			fa_pose_blend_linear(&outPose, &actionPose, &outPose, currAlpha);
			fa_pose_stack_pop(ctx->poseStack, 1);
		}
	}
	
	// animate next action
	if(nextAction->fnUpdate != NULL)
	{
		fa_action_t* action = nextAction;
		fa_character_action_animate(character, layer, action, ctx);
		
		if(layer->actionQueue.cachePoseAfterNextAction)
		{
			fa_character_layer_cache_pose(layer, ctx, nextAlpha);
		}
		
		// blend in result
		{
			fa_pose_t actionPose;
			fa_pose_t outPose;
			fa_pose_stack_get(ctx->poseStack, &actionPose, 0);
			fa_pose_stack_get(ctx->poseStack, &outPose, 1);
			fa_pose_blend_linear(&outPose, &actionPose, &outPose, nextAlpha);
			fa_pose_stack_pop(ctx->poseStack, 1);
		}
	}
	
	// only body layer can affect legs IK, it's secured outside this function
	{
		const float currIK = currAction->args.ikMode == FA_IK_MODE_LEGS ? currAlpha : 0.0f;
		const float nextIK = nextAction->args.ikMode == FA_IK_MODE_LEGS ? 1.0f : 0.0f;
		
		const float weightIK = currIK * (1.0f - nextAlpha) + nextIK * nextAlpha;
		ctx->outWeightLegsIK = weightIK;
	}
	
	fa_action_queue_resolve_post_animate(character, &layer->actionQueue);
}

void fa_character_ik(fa_character_t* character, fa_cross_layer_context_t* layerCtx)
{
	// inverse kinematics
	{
		const float weightIK = layerCtx->outWeightLegsIK;
		
		if(weightIK > 0.0f)
		{
			fa_pose_stack_push(layerCtx->poseStack, 1);
			
			fa_pose_t poseMS;
			fa_pose_stack_get(layerCtx->poseStack, &poseMS, 0);
			fa_pose_t poseLS;
			fa_pose_stack_get(layerCtx->poseStack, &poseLS, 1);
			
			fa_pose_local_to_model(&poseMS, &poseLS, character->rig->parents);
			
			fm_vec4 leftTarget = poseMS.xforms[character->rig->ikLeftLeg.idxEnd].pos;
			leftTarget.z = -0.2f;
			fm_vec4 rightTarget = poseMS.xforms[character->rig->ikRightLeg.idxEnd].pos;
			rightTarget.z = 0.0f;
			
			// pelvis height correction
			{
				fm_vec4 leftHipPos = poseMS.xforms[character->rig->ikLeftLeg.idxBeginParent].pos;
				fm_vec4 rightHipPos = poseMS.xforms[character->rig->ikRightLeg.idxBeginParent].pos;
				fm_vec4 leftTargetDir;
				fm_vec4_sub(&leftTarget, &leftHipPos, &leftTargetDir);
				fm_vec4 rightTargetDir;
				fm_vec4_sub(&rightTarget, &rightHipPos, &rightTargetDir);
				
				const float leftDistance = fm_vec4_mag(&leftTargetDir);
				const float rightDistance = fm_vec4_mag(&rightTargetDir);
				fm_vec4 leftLegVec = poseLS.xforms[character->rig->ikLeftLeg.idxMid].pos;
				fm_vec4_add(&leftLegVec, &poseLS.xforms[character->rig->ikLeftLeg.idxEnd].pos, &leftLegVec);
				fm_vec4 rightLegVec = poseLS.xforms[character->rig->ikRightLeg.idxMid].pos;
				fm_vec4_add(&rightLegVec, &poseLS.xforms[character->rig->ikRightLeg.idxEnd].pos, &rightLegVec);
				
				const float footCorrectionDistance = 0.15f;
				
				const float leftLegLength = fm_vec4_mag(&leftLegVec) + footCorrectionDistance;
				const float rightLegLength = fm_vec4_mag(&rightLegVec) + footCorrectionDistance;
				
				float pelvisCorrectionHeight = 0.0f;
				if(leftLegLength < leftDistance)
					pelvisCorrectionHeight = leftDistance - leftLegLength;
				if(rightLegLength < (rightDistance - pelvisCorrectionHeight))
					pelvisCorrectionHeight = rightDistance - rightLegLength;
				
				poseLS.xforms[character->rig->ikLeftLeg.idxBeginParent].pos.y -= pelvisCorrectionHeight * weightIK;
				fa_pose_local_to_model(&poseMS, &poseLS, character->rig->parents);
			}
			
			fa_character_leg_ik(character, &character->rig->ikLeftLeg, &poseLS, &poseMS, &leftTarget, weightIK);
			fa_character_leg_ik(character, &character->rig->ikRightLeg, &poseLS, &poseMS, &rightTarget, weightIK);
			
			fa_pose_stack_pop(layerCtx->poseStack, 1);
		}
	}
}

void fa_character_look_at(fa_character_t* character, fa_cross_layer_context_t* layerCtx)
{
	if(layerCtx->lookAtWeight <= 0.0f)
		return;
	
	const fa_look_at_setup_t* lookAt = &character->rig->headLookAt;
	const fm_vec4 lookAtPosition = {layerCtx->lookAtLocator.x, layerCtx->lookAtLocator.y, layerCtx->lookAtLocator.z, 1.0f};
	
	fa_pose_stack_t* poseStack = layerCtx->poseStack;
	
	// push temporary MS pose
	fa_pose_stack_push(poseStack, 1);
	
	// get LS and calculate temporary MS pose
	fa_pose_t poseMS;
	fa_pose_stack_get(poseStack, &poseMS, 0);
	fa_pose_t poseLS;
	fa_pose_stack_get(poseStack, &poseLS, 1);
	
	fa_pose_local_to_model(&poseMS, &poseLS, character->rig->parents);
	
	// calculate look-at vector and forward vector
	fm_xform headLocator = poseMS.xforms[lookAt->idxHead];
	fm_vec4 headDirection = fm_quat_axis_x(&headLocator.rot);
	fm_vec4 lookAtDirection = {};
	fm_vec4_sub(&lookAtPosition, &headLocator.pos, &lookAtDirection);
	
	if(layerCtx->debug != NULL)
	{
		float cyan[4] = FUR_COLOR_CYAN;
		float yellow[4] = FUR_COLOR_YELLOW;
		fc_dbg_line(&headLocator.pos.x, &lookAtPosition.x, cyan);
		
		fm_vec4 headForwardPoint = {};
		fm_vec4_add(&headLocator.pos, &headDirection, &headForwardPoint);
		
		fc_dbg_line(&headLocator.pos.x, &headForwardPoint.x, yellow);
	}
	
	fm_quat lookAtSpace = headLocator.rot;
	fm_quat_conj(&lookAtSpace);
	
	fm_quat_rot(&lookAtSpace, &headDirection, &headDirection);
	fm_quat_rot(&lookAtSpace, &lookAtDirection, &lookAtDirection);
	
	fm_quat identity = {};
	fm_quat_identity(&identity);
	
	fm_quat lookAtRotCorrection = {};
	fm_vec4_rot_between(&headDirection, &lookAtDirection, &lookAtRotCorrection);
	
	fm_quat_slerp(&identity, &lookAtRotCorrection, layerCtx->lookAtWeight, &lookAtRotCorrection);
	
	fm_quat_mul(&lookAtRotCorrection, &poseLS.xforms[lookAt->idxHead].rot, &poseLS.xforms[lookAt->idxHead].rot);
	
	// pop temporary MS pose
	fa_pose_stack_pop(poseStack, 1);
}

void fa_character_animate(fa_character_t* character, const fa_character_animate_ctx_t* ctx)
{
	// allocate pose stack and command buffer memory
	uint32_t scratchpadBufferSizeUsed = 0;
	void* scratchpadBufferPtr = ctx->scratchpadBuffer;
	
	const uint32_t poseStackSize = 128 * 1024;
	void* animPoseStackMemory = NULL;
	{
		uint32_t sizeRequired = poseStackSize;
		FUR_ASSERT(scratchpadBufferSizeUsed + sizeRequired < ctx->scratchpadBufferSize);
		
		animPoseStackMemory = scratchpadBufferPtr;
		
		uint8_t* ptr = (uint8_t*)scratchpadBufferPtr;
		ptr += sizeRequired;
		scratchpadBufferPtr = (void*)ptr;
		scratchpadBufferSizeUsed += sizeRequired;
	}
	
	const uint32_t animCmdBufferSize = 32 * 1024;
	void* animCmdBufferMemory = NULL;
	{
		uint32_t sizeRequired = animCmdBufferSize;
		FUR_ASSERT(scratchpadBufferSizeUsed + sizeRequired < ctx->scratchpadBufferSize);
		
		animCmdBufferMemory = scratchpadBufferPtr;
		
		uint8_t* ptr = (uint8_t*)scratchpadBufferPtr;
		ptr += sizeRequired;
		scratchpadBufferPtr = (void*)ptr;
		scratchpadBufferSizeUsed += sizeRequired;
	}
	
	// init pose stack - pose stack is shared across multiple command buffers
	fa_pose_stack_t poseStack = {};
	
	{
		fa_pose_stack_desc_t desc = {};
		
		desc.numBonesPerPose = character->rig->numBones;
		desc.numTracksPerPose = 0;
		desc.numMaxPoses = 4;
		
		fa_pose_stack_init(&poseStack, &desc, animPoseStackMemory, poseStackSize);
	}
	
	fa_cmd_context_debug_t debug = {};
	
	// update layers
	fa_cross_layer_context_t layerCtx = {};
	layerCtx.dt = ctx->dt;
	layerCtx.poseStack = &poseStack;
	layerCtx.scratchMemory = animCmdBufferMemory;
	layerCtx.scratchMemorySize = animCmdBufferSize;
	layerCtx.debug = ctx->showDebug ? &debug : NULL;
	
	fa_pose_t poseLS;
	
	// reset pose to ref pose
	fa_pose_stack_push(&poseStack, 1);
	fa_pose_stack_get(&poseStack, &poseLS, 0);
	{
		FUR_ASSERT(poseLS.numXforms == character->rig->numBones);
		
		for(uint32_t i=0; i<poseLS.numXforms; ++i)
		{
			poseLS.xforms[i] = character->rig->refPose[i];
			poseLS.weightsXforms[i] = 255;
		}
		
		for(uint32_t i=0; i<poseLS.numTracks; ++i)
		{
			poseLS.tracks[i] = 0.0f;
			poseLS.weightsTracks[i] = 255;
		}
	}
	
	// body
	FUR_PROFILE("body-layer")
	{
		fa_character_layer_animate(character, &layerCtx, &character->layerFullBody);
	}
	
	// store root motion
	const float rootMotionDeltaX = character->animInfo.rootMotionDeltaX;
	const float rootMotionDeltaY = character->animInfo.rootMotionDeltaY;
	const float rootMotionDeltaYaw = character->animInfo.rootMotionDeltaYaw;
	
	// store weight IK
	const float weightLegsIK = layerCtx.outWeightLegsIK;
	
	// partial layer, can be applied anywhere, but does not interrupt full body
	FUR_PROFILE("partial-layer")
	{
		fa_character_layer_animate(character, &layerCtx, &character->layerPartial);
	}
	
	// face
	FUR_PROFILE("face-layer")
	{
		fa_character_layer_animate(character, &layerCtx, &character->layerFace);
	}
	
	// inverse kinematics
	FUR_PROFILE("ik")
	{
		layerCtx.outWeightLegsIK = weightLegsIK;
		fa_character_ik(character, &layerCtx);
	}
	
	// look-at
	FUR_PROFILE("look-at")
	{
		static float time = 0.0f;
		time += ctx->dt;
		
		layerCtx.lookAtLocator.x = 2.0f * sinf(time * 2.0f + 1.0f);
		layerCtx.lookAtLocator.y = -4.0f;
		layerCtx.lookAtLocator.z = 2.0f + 2.0f * sinf(time);
		
		layerCtx.lookAtWeight = 0.0f;	// change to non-zero to enable look-at
		
		fa_character_look_at(character, &layerCtx);
	}
	
	// ragdoll

	// apply root motion
	character->animInfo.rootMotionDeltaX = rootMotionDeltaX;
	character->animInfo.rootMotionDeltaY = rootMotionDeltaY;
	character->animInfo.rootMotionDeltaYaw = rootMotionDeltaYaw;
	
	// convert to model space
	FUR_PROFILE("ls-to-ms")
	{
		const int16_t* parentIndices = character->rig->parents;
		fa_pose_t poseMS = {};
		poseMS.xforms = character->poseMS;
		poseMS.numXforms = character->rig->numBones;
		fa_pose_local_to_model(&poseMS, &poseLS, parentIndices);
	}
}

void fa_action_animate_func(const fa_action_ctx_t* ctx, void* userData)
{
	// player motion update
	fa_character_anim_info_t* animInfo = ctx->animInfo;
	
	// we know we play it on full-body layer
	animInfo->rootMotionDeltaX = 0.0f;
	animInfo->rootMotionDeltaY = 0.0f;
	
	// animation update
	fa_action_animate_t* data = (fa_action_animate_t*)userData;
	
	const float animDuration = data->animation->duration;
	const float time = fmodf(ctx->localTime, animDuration);
	fa_cmd_anim_sample(ctx->cmdRecorder, time, 0);
	
	data->progress = time / animDuration;
}

const fa_anim_clip_t** fa_action_animate_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_animate_t* data = (const fa_action_animate_t*)userData;
	*numAnims = 1;
	return (const fa_anim_clip_t**)&data->animation;	// todo: check it, is this return correct?
}

void fa_action_animate_begin_func(const fa_action_begin_end_ctx_t* ctx, void* userData)
{
	// ...
}

void fa_action_animate_end_func(const fa_action_begin_end_ctx_t* ctx, void* userData)
{
	fa_action_animate_t* data = (fa_action_animate_t*)userData;
	data->reserved = false;
}

void fa_action_animate_cancel_func(void* userData)
{
	fa_action_animate_t* data = (fa_action_animate_t*)userData;
	data->reserved = false;
}

fa_layer_t* fa_character_layer_select(fa_character_t* character, const fa_action_args_t* args)
{
	if(args->layerName == 0)
	{
		switch(args->layer)
		{
			case FA_CHAR_LAYER_FULL_BODY:
				return &character->layerFullBody;
			case FA_CHAR_LAYER_PARTIAL:
				return &character->layerPartial;
			default:
				return NULL;
		}
	}
	
	if(args->layerName == SID("full-body"))
		return &character->layerFullBody;
	else if(args->layerName == SID("partial"))
		return &character->layerPartial;
	else if(args->layerName == SID("face"))
		return &character->layerFace;
	
	return NULL;
}

void fa_character_schedule_action_simple(fa_character_t* character, fa_action_animate_t* action, const fa_action_args_t* args)
{
	fa_layer_t* layer = fa_character_layer_select(character, args);
	FUR_ASSERT(layer);
	
	fa_action_t* actionSlot = fa_action_queue_get_free_slot(&layer->actionQueue);
	actionSlot->userData = action;
	actionSlot->fnUpdate = fa_action_animate_func;
	actionSlot->fnGetAnims = fa_action_animate_get_anims_func;
	actionSlot->fnBegin = fa_action_animate_begin_func;
	actionSlot->fnEnd = fa_action_animate_end_func;
	actionSlot->fnCancel = fa_action_animate_cancel_func;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}

CANIM_API void fa_character_schedule_none_action(fa_character_t* character, const fa_action_args_t* args)
{
	fa_layer_t* layer = fa_character_layer_select(character, args);
	FUR_ASSERT(layer);
	
	fa_action_t* actionSlot = fa_action_queue_get_free_slot(&layer->actionQueue);
	actionSlot->userData = NULL;
	actionSlot->fnUpdate = NULL;
	actionSlot->fnGetAnims = NULL;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}

CANIM_API void fa_character_schedule_action(fa_character_t* character, fa_action_schedule_data_t* data, const fa_action_args_t* args)
{
	fa_layer_t* layer = fa_character_layer_select(character, args);
	FUR_ASSERT(layer);
	
	fa_action_t* actionSlot = fa_action_queue_get_free_slot(&layer->actionQueue);
	actionSlot->userData = data->userData;
	actionSlot->fnUpdate = data->fnUpdate;
	actionSlot->fnGetAnims = data->fnGetAnims;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}

// -----

void fa_action_animate_test_func(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_animate_test_t* data = (fa_action_animate_test_t*)userData;
	
	const float d_0 = data->anims[0]->duration;
	const float t_0 = fmodf(ctx->localTime, d_0);
	
	const float d_1 = data->anims[1]->duration;
	const float t_1 = fmodf(ctx->localTime, d_1);
	
#if 0
	fa_cmd_anim_sample(ctx->cmdRecorder, t_0, 0);
	fa_cmd_anim_sample(ctx->cmdRecorder, t_1, 1);
	fa_cmd_apply_mask(ctx->cmdRecorder, FA_MASK_UPPER_BODY);
	fa_cmd_blend2(ctx->cmdRecorder, 1.0f);
#elif 1
	const float alpha = fm_clamp(ctx->localTime - data->timeToNextAnim, 0.0f, 0.5f) * 2.0f;
	fa_cmd_anim_sample(ctx->cmdRecorder, t_0, 0);
	fa_cmd_anim_sample(ctx->cmdRecorder, t_1, 1);
	fa_cmd_apply_mask(ctx->cmdRecorder, FA_MASK_UPPER_BODY);
	fa_cmd_blend2(ctx->cmdRecorder, alpha);
	
	if(alpha >= 0.1f)
	{
		data->equipWeapon = true;
	}
#elif 0
	if(ctx->localTime < 2.0f)
	{
		fa_cmd_identity(ctx->cmdRecorder);
	}
	else
	{
		const float alpha = fm_clamp(ctx->localTime - 2.0f, 0.0f, 1.0f);
		fa_cmd_identity(ctx->cmdRecorder);
		fa_cmd_anim_sample(ctx->cmdRecorder, 0.0f, 0);
		fa_cmd_blend2(ctx->cmdRecorder, alpha);
	}
#else
	fa_cmd_anim_sample(ctx->cmdRecorder, t_0, 0);
	fa_cmd_anim_sample_additive(ctx->cmdRecorder, t_1, 1);
	fa_cmd_apply_additive(ctx->cmdRecorder, 1.0f);
#endif
}

const fa_anim_clip_t** fa_action_animate_test_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_animate_test_t* data = (const fa_action_animate_test_t*)userData;
	*numAnims = 2;
	return (const fa_anim_clip_t**)&data->anims;	// todo: check it, is this return correct?
}

void fa_character_schedule_action_test_simple(fa_character_t* character, fa_action_animate_test_t* action, const fa_action_args_t* args)
{
	fa_layer_t* layer = fa_character_layer_select(character, args);
	FUR_ASSERT(layer);
	
	fa_action_t* actionSlot = fa_action_queue_get_free_slot(&layer->actionQueue);
	actionSlot->userData = action;
	actionSlot->fnUpdate = fa_action_animate_test_func;
	actionSlot->fnGetAnims = fa_action_animate_test_get_anims_func;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->args = *args;
}

// -----

void fa_action_player_loco_update(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_player_loco_t* data = (fa_action_player_loco_t*)userData;
	
	// update player motion
	fa_character_anim_info_t* animInfo = ctx->animInfo;
	
	const bool doMove = fabs(animInfo->desiredMoveX) > 0.05f || fabs(animInfo->desiredMoveY) > 0.05f;
	if(doMove)
	{
		const float dirMag = sqrtf(animInfo->desiredMoveX * animInfo->desiredMoveX + animInfo->desiredMoveY * animInfo->desiredMoveY);
		const float dirX = animInfo->desiredMoveX / dirMag;
		const float dirY = animInfo->desiredMoveY / dirMag;
		const float angle = -fm_sign(dirY) * acosf(dirX) - FM_PI / 2.0f;
		animInfo->rootMotionDeltaYaw = angle - animInfo->currentYaw;
		animInfo->currentYaw = angle;
		animInfo->rootMotionDeltaX = animInfo->desiredMoveX * ctx->dt;
		animInfo->rootMotionDeltaY = animInfo->desiredMoveY * ctx->dt;
		
		data->isStopping = false;
		
		if(data->blendState == 0.0f)
		{
			data->runLocalTime = 0.0f;
		}
		else
		{
			const float d_0 = data->anims[FA_ACTION_PLAYER_LOCO_ANIM_RUN]->duration;
			data->runLocalTime = fmodf(data->runLocalTime + ctx->dt, d_0);
		}
		
		if(data->blendState < 1.0f)
		{
			data->blendState = fm_clamp(data->blendState + ctx->dt / 0.3f, 0.0f, 1.0f);
		}
	}
	else
	{
		if(data->blendState == 1.0f)
		{
			data->idleLocalTime = 0.0f;
			data->isStopping = true;
		}
		else
		{
			const float d_0 = data->anims[FA_ACTION_PLAYER_LOCO_ANIM_RUN_TO_IDLE_SHARP]->duration;
			data->idleLocalTime = fm_clamp(data->idleLocalTime + ctx->dt, 0.0f, d_0);
		}
		
		if(data->blendState > 0.0f)
		{
			data->blendState = fm_clamp(data->blendState - ctx->dt / 0.3f, 0.0f, 1.0f);
		}
	}
	
	if(0.0f < data->blendState && data->blendState < 1.0f)
	{
		if(!data->isStopping)
		{
			fa_cmd_anim_sample(ctx->cmdRecorder, data->idleLocalTime, FA_ACTION_PLAYER_LOCO_ANIM_IDLE);
		}
		else
		{
			fa_cmd_anim_sample(ctx->cmdRecorder, data->idleLocalTime, FA_ACTION_PLAYER_LOCO_ANIM_RUN_TO_IDLE_SHARP);
		}
		
		fa_cmd_anim_sample(ctx->cmdRecorder, data->runLocalTime, FA_ACTION_PLAYER_LOCO_ANIM_RUN);
		fa_cmd_blend2(ctx->cmdRecorder, fm_curve_uniform_s(data->blendState));
	}
	else if(data->blendState == 1.0f)
	{
		fa_cmd_anim_sample(ctx->cmdRecorder, data->runLocalTime, FA_ACTION_PLAYER_LOCO_ANIM_RUN);
	}
	else
	{
		fa_cmd_anim_sample(ctx->cmdRecorder, data->idleLocalTime, FA_ACTION_PLAYER_LOCO_ANIM_RUN_TO_IDLE_SHARP);
	}
	
}

const fa_anim_clip_t** fa_action_player_loco_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_player_loco_t* data = (const fa_action_player_loco_t*)userData;
	*numAnims = FA_ACTION_PLAYER_LOCO_ANIM_COUNT;
	return (const fa_anim_clip_t**)&data->anims;	// todo: check it, is this return correct?
}

// -----

void fa_action_player_jump_update(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_player_jump_t* data = (fa_action_player_jump_t*)userData;
	
	// update player motion
	fa_character_anim_info_t* animInfo = ctx->animInfo;
	
	const float t = ctx->localTime;
	const bool doMove = fabs(animInfo->desiredMoveX) > 0.05f || fabs(animInfo->desiredMoveY) > 0.05f;
	
	if(data->jumpType == 0)
	{
		data->jumpType = doMove ? 2 : 1;
	}
	
	if(data->jumpType == 1) // jump in place
	{
		const float d = data->anims[0]->duration;
		const float t_anim = fm_clamp(t, 0.0f, d);
		
		data->progress = t / d;
		
		fa_cmd_anim_sample(ctx->cmdRecorder, t_anim, 0);
	}
	else // jump in run
	{
		const float d = data->anims[1]->duration;
		const float t_anim = fm_clamp(t, 0.0f, d);
		
		data->progress = t / d;
		
		if(doMove)
		{
			const float dirMag = sqrtf(animInfo->desiredMoveX * animInfo->desiredMoveX + animInfo->desiredMoveY * animInfo->desiredMoveY);
			const float dirX = animInfo->desiredMoveX / dirMag;
			const float dirY = animInfo->desiredMoveY / dirMag;
			
			const float angle = -fm_sign(dirY) * acosf(dirX) - FM_PI / 2.0f;
			const float yawNewPercentage = 0.05f;
			animInfo->currentYaw = angle;
			
			animInfo->rootMotionDeltaX = animInfo->rootMotionDeltaX * (1.0f - yawNewPercentage) + animInfo->desiredMoveX * yawNewPercentage * ctx->dt;
			animInfo->rootMotionDeltaY = animInfo->rootMotionDeltaY * (1.0f - yawNewPercentage) + animInfo->desiredMoveY * yawNewPercentage * ctx->dt;
		}
		
		fa_cmd_anim_sample(ctx->cmdRecorder, t_anim, 1);
	}
}

const fa_anim_clip_t** fa_action_player_jump_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_player_jump_t* data = (const fa_action_player_jump_t*)userData;
	*numAnims = 2;
	return (const fa_anim_clip_t**)&data->anims;	// todo: check it, is this return correct?
}

// -----

void fa_action_player_loco_start_update(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_player_loco_start_t* data = (fa_action_player_loco_start_t*)userData;
	const float t = ctx->localTime;
	const float d = data->anims[0]->duration;
	
	// update player motion
	fa_character_anim_info_t* animInfo = ctx->animInfo;
	
	const bool doMove = fabs(animInfo->desiredMoveX) > 0.05f || fabs(animInfo->desiredMoveY) > 0.05f;
	if(doMove)
	{
		const float dirMag = sqrtf(animInfo->desiredMoveX * animInfo->desiredMoveX + animInfo->desiredMoveY * animInfo->desiredMoveY);
		const float dirX = animInfo->desiredMoveX / dirMag;
		const float dirY = animInfo->desiredMoveY / dirMag;
		const float angle = -fm_sign(dirY) * acosf(dirX) - FM_PI / 2.0f;
		
		const float motionCurve = fm_curve_uniform_s(fm_clamp(2.0f * t / d, 0.0f, 1.0f));
		
		if(!data->ignoreYaw)
		{
			animInfo->rootMotionDeltaYaw = angle - animInfo->currentYaw;
			animInfo->currentYaw = animInfo->currentYaw * (1.0f - motionCurve) + motionCurve * angle;
		}
		
		animInfo->rootMotionDeltaX = animInfo->desiredMoveX * ctx->dt * motionCurve;
		animInfo->rootMotionDeltaY = animInfo->desiredMoveY * ctx->dt * motionCurve;
	}
	
	// animate start
	const float t_anim = fm_clamp(t, 0.0f, d);
	
	data->isFinished = t > (d - data->finishFromEnd);
	
	fa_cmd_anim_sample(ctx->cmdRecorder, t_anim, 0);
}

const fa_anim_clip_t** fa_action_player_loco_start_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_player_loco_start_t* data = (const fa_action_player_loco_start_t*)userData;
	*numAnims = 1;
	return (const fa_anim_clip_t**)&data->anims;	// todo: check it, is this return correct?
}

// -----

void fa_dangle_simulate_single_step(fa_dangle* dangle, float dt)
{
	const uint32_t count = dangle->numParaticles;
	
	const fm_vec4 gravity = {0.0f, 0.0f, dt * -5.0f};
	const float damping_coef = dangle->damping;
	
	dangle->p[0] = dangle->x0[0];
	
	for(uint32_t i=1; i<count; ++i)	// start from 1, as 0 is attach point
	{
		// v = v + dt * g
		fm_vec4_add(&dangle->v[i], &gravity, &dangle->v[i]);
		
		// damping velocity
		fm_vec4_mulf(&dangle->v[i], damping_coef, &dangle->v[i]);
		
		// p = x0 + dt * v
		fm_vec4 vel = dangle->v[i];
		fm_vec4_mulf(&vel, dt, &vel);
		fm_vec4_add(&dangle->x0[i], &vel, &dangle->p[i]);
	}
	
	const uint32_t numIterations = 4;
	for(uint32_t it=0; it<numIterations; ++it)
	{
		for(uint32_t i=1; i<count; ++i)	// start from 1, as 0 is attach point
		{
			const fm_vec4 p0 = dangle->p[i-1];
			const fm_vec4 p1 = dangle->p[i];
			
			// distance constraint
			const float refDistance = dangle->d[i-1];
			fm_vec4 disp;
			fm_vec4_sub(&p1, &p0, &disp);
			
			const float distance = fm_vec4_mag(&disp);
			fm_vec4_normalize(&disp);
			
			const float constraintDist = refDistance - distance;
			fm_vec4_mulf(&disp, constraintDist, &disp);
			
			fm_vec4_add(&dangle->p[i], &disp, &dangle->p[i]);
			
			// sphere collision constraint
			if(dangle->spherePos)
			{
				const fm_vec4 spherePos = *dangle->spherePos;
				const float sphereRadius = dangle->sphereRadius;
				
				fm_vec4 sphereDir;
				fm_vec4_sub(&dangle->p[i], &spherePos, &sphereDir);
				float sphereDist = fm_vec4_mag(&sphereDir);
				if(sphereDist < sphereRadius)
				{
					fm_vec4_normalize(&sphereDir);
					fm_vec4_mulf(&sphereDir, sphereRadius - sphereDist, &sphereDir);
					fm_vec4_add(&dangle->p[i], &sphereDir, &dangle->p[i]);
				}
			}
		}
	}
	
	const float inv_dt = (dt > 0.00000001f) ? 1.0f / dt : 0.0f;
	for(uint32_t i=1; i<count; ++i)	// start from 1, as 0 is attach point
	{
		fm_vec4_sub(&dangle->p[i], &dangle->x0[i], &dangle->v[i]);
		fm_vec4_mulf(&dangle->v[i], inv_dt, &dangle->v[i]);
		dangle->x0[i] = dangle->p[i];
	}
}

void fa_dangle_create(const fa_dangle_desc* desc, fa_dangle* dangle, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_ASSERT(!dangle->x0 && !dangle->p && !dangle->v && !dangle->d);
	
	dangle->x0 = FUR_ALLOC_ARRAY_AND_ZERO(fm_vec4, desc->numParticles, 16, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	dangle->p = FUR_ALLOC_ARRAY_AND_ZERO(fm_vec4, desc->numParticles, 16, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	dangle->v = FUR_ALLOC_ARRAY_AND_ZERO(fm_vec4, desc->numParticles, 16, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	dangle->d = FUR_ALLOC_ARRAY_AND_ZERO(float, desc->numParticles-1, 16, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	
	dangle->freq = desc->frequency;
	dangle->numParaticles = desc->numParticles;
	dangle->tAcc = 0.0f;
	dangle->damping = desc->dampingCoef;
}

void fa_dangle_release(fa_dangle* dangle, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_ASSERT(dangle->x0 && dangle->p && dangle->v && dangle->d);
	
	FUR_FREE(dangle->x0, pAllocCallbacks);
	FUR_FREE(dangle->p, pAllocCallbacks);
	FUR_FREE(dangle->v, pAllocCallbacks);
	FUR_FREE(dangle->d, pAllocCallbacks);
	
	dangle->x0 = NULL;
	dangle->p = NULL;
	dangle->v = NULL;
	dangle->d = NULL;
}

void fa_dangle_simulate(const fa_dangle_sim_ctx* ctx, fa_dangle* dangle)
{
	dangle->tAcc += ctx->dt;
	const float timeStep = 1.0f / dangle->freq;
	
	while(dangle->tAcc >= timeStep)
	{
		dangle->tAcc -= timeStep;
		fa_dangle_simulate_single_step(dangle, timeStep);
	}
}

void fa_dangle_to_matrices_z_up(const fa_dangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices)
{
	const uint32_t count = dangle->numParaticles - 1;
	const fm_vec4* p = dangle->p;
	
	matrices[0] = *attachmentMatrix;
	matrices[0].w = p[0];
	matrices[0].w.w = 1.0f;
	
	fm_vec4 refDir = attachmentMatrix->x;
	
	for(uint32_t i=1; i<count; ++i)
	{
		fm_vec4 z;
		fm_vec4_sub(&p[i], &p[i+1], &z);
		fm_vec4_normalize(&z);
		
		fm_vec4 y;
		fm_vec4_cross(&z, &refDir, &y);
		fm_vec4_normalize(&y);
		
		fm_vec4 x;
		fm_vec4_cross(&y, &z, &x);
		fm_vec4_normalize(&x);
		
		matrices[i].x = x;
		matrices[i].y = y;
		matrices[i].z = z;
		matrices[i].w = p[i];
		matrices[i].w.w = 1.0f;
		
		refDir = matrices[i].x;
	}
	
	matrices[count] = matrices[count-1];
	matrices[count].w = p[count];
	matrices[count].w.w = 1.0f;
}

void fa_dangle_to_matrices_y_down(const fa_dangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices)
{
	const uint32_t count = dangle->numParaticles - 1;
	const fm_vec4* p = dangle->p;
	
	fm_vec4 refDir = attachmentMatrix->x;
	
	for(uint32_t i=0; i<count; ++i)
	{
		fm_vec4 y;
		fm_vec4_sub(&p[i+1], &p[i], &y);
		fm_vec4_normalize(&y);
		
		fm_vec4 z;
		fm_vec4_cross(&refDir, &y, &z);
		fm_vec4_normalize(&z);
		
		fm_vec4 x;
		fm_vec4_cross(&y, &z, &x);
		fm_vec4_normalize(&x);
		
		matrices[i].x = x;
		matrices[i].y = y;
		matrices[i].z = z;
		matrices[i].w = p[i];
		matrices[i].w.w = 1.0f;
		
		refDir = matrices[i].x;
	}
	
	matrices[count] = matrices[count-1];
	matrices[count].w = p[count];
	matrices[count].w.w = 1.0f;
}

void fm_axis_to_vec4(fm_axis_t axis, fm_vec4* v)
{
	static fm_vec4 axes[6] = {
		{1, 0, 0, 0},
		{0, 1, 0, 0},
		{0, 0, 1, 0},
		{-1, 0, 0, 0},
		{0, -1, 0, 0},
		{0, 0, -1, 0},
	};
	
	*v = axes[axis];
}
