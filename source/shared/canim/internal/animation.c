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
	
	FUR_FREE(rig, pAllocCallbacks);
}

void fa_anim_clip_release(fa_anim_clip_t* clip, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(clip->curves, pAllocCallbacks);
	FUR_FREE(clip->dataKeys, pAllocCallbacks);
	
	FUR_FREE(clip, pAllocCallbacks);
}


// -----

void fa_pose_set_identity(fa_pose_t* pose)
{
	fm_xform* xforms = pose->xforms;
	float* tracks = pose->tracks;
	
	const uint8_t* weightsXformsBegin = pose->weightsXforms;
	const uint8_t* weightsTracksBegin = pose->weightsTracks;
	
	const uint8_t* weightsXforms = weightsXformsBegin;
	const uint8_t* weightsTracks = weightsTracksBegin;
	
	for(uint32_t i=0; i<pose->numXforms; ++i)
	{
		const uint8_t weight = weightsXformsBegin ? *weightsXforms : 255;
		
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

void fa_pose_set_reference(const fa_rig_t* rig, fa_pose_t* pose)
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
		const uint8_t weight = weightsXformsBegin ? *weightsXforms : 255;
		
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

static inline uint16_t fa_anim_clip_key_get_bone_index(const fa_anim_curve_t* curve)
{
	return curve->index & 0x3fff;
}

static inline uint16_t fa_anim_clip_key_get_channel(const fa_anim_curve_t* curve)
{
	return (curve->index & 0xc000) >> 14;
}

const float Km  = 4.0*(0.4142135679721832275390625); // 4(sqrt(2)-1)
const float Khf = 2.414213657379150390625;           // sqrt(2)+1 = 1/(sqrt(2)-1)
const float Khi = 0.17157287895679473876953125;      // 3-2sqrt(2)

float fa_decompress_float_minus_one_plus_one(uint16_t value)
{
	return (((float)value) / 65535.0f) * 2.0f - 1.0f;
}

uint16_t fa_compress_float_minus_one_plus_on(float value)
{
	return (uint16_t)(((value + 1.0f) / 2.0f) * 65535.0f);
}

void fm_vec3_to_16bit(const fm_vec3* v, uint16_t* b)
{
	b[0] = fa_compress_float_minus_one_plus_on(v->x);
	b[1] = fa_compress_float_minus_one_plus_on(v->y);
	b[2] = fa_compress_float_minus_one_plus_on(v->z);
}

void fm_16bit_to_vec3(const uint16_t* b, fm_vec3* v)
{
	v->x = fa_decompress_float_minus_one_plus_one(b[0]);
	v->y = fa_decompress_float_minus_one_plus_one(b[1]);
	v->z = fa_decompress_float_minus_one_plus_one(b[2]);
}

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

void vec4_com_16bit(fm_vec4 v, uint16_t* b)
{
	fm_vec3 vec = {v.x / 10.0f, v.y / 10.0f, v.z / 10.0f};
	fm_vec3_to_16bit(&vec, b);
}

fm_vec4 vec4_decom_16bit(const uint16_t* v)
{
	fm_vec3 vec;
	fm_16bit_to_vec3(v, &vec);
	
	fm_vec4 res;
	res.x = vec.x * 10.0f;
	res.y = vec.y * 10.0f;
	res.z = vec.z * 10.0f;
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

void fa_anim_clip_sample(const fa_anim_clip_t* clip, float time, fa_pose_t* pose)
{
	const uint32_t numCurves = clip->numCurves;
	
	for(uint32_t i_c=0; i_c<numCurves; ++i_c)
	{
		const fa_anim_curve_t* curve = &clip->curves[i_c];
		
		const uint16_t idxXform = curve->index;
		
		// rotation
		{
			const uint16_t numKeys = curve->numKeys;
		
			uint16_t idx = 0;
			
			// todo: make it a binary search (or a binary-guess search, check weekly links)
			while(idx < (numKeys-1) && fa_decompress_key_time(curve->keys[idx].keyTime) < time)
			{
				++idx;
			}
			
			const uint16_t upperIdx = idx;
			const uint16_t lowerIdx = idx == 0 ? idx : idx - 1;
			
			fm_quat rot;
			
			if(lowerIdx == upperIdx)
			{
				fa_decompress_rotation_key(&curve->keys[idx], &rot);
			}
			else
			{
				fm_quat rot1;
				fa_decompress_rotation_key(&curve->keys[lowerIdx], &rot1);
				
				fm_quat rot2;
				fa_decompress_rotation_key(&curve->keys[upperIdx], &rot2);
				
				const float time1 = fa_decompress_key_time(curve->keys[lowerIdx].keyTime);
				const float time2 = fa_decompress_key_time(curve->keys[upperIdx].keyTime);
				
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
			
			// todo: make it a binary search (or a binary-guess search, check weekly links)
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
				fm_vec4_lerp(&pos1, &pos2, alpha, &pos);
			}
			
			pose->xforms[idxXform].pos = pos;
		}
		
		pose->weightsXforms[idxXform] = 255;
	}
	
	for(uint32_t i=numCurves; i<pose->numXforms; ++i)
	{
		pose->weightsXforms[i] = 0;
	}
	
	for(uint32_t i=0; i<pose->numTracks; ++i)
	{
		pose->weightsTracks[i] = 0;
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
			//fm_xform_mul(&localXforms[i], &modelXforms[idxParent], &modelXforms[i]);
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

#define FA_DBG_TEXT_X -400.0f
#define FA_DBG_TEXT_Y(_pos) 1.0f - 1.0f * _pos

// set reference pose command
fa_cmd_status_t fa_cmd_impl_ref_pose(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_pose_stack_push(ctx->poseStack, 1);
	
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	FUR_ASSERT(pose.numXforms == ctx->rig->numBones);
	
	for(uint32_t i=0; i<pose.numXforms; ++i)
	{
		pose.xforms[i] = ctx->rig->refPose[i];
		pose.weightsXforms[i] = 255;
	}
	
	for(uint32_t i=0; i<pose.numTracks; ++i)
	{
		pose.tracks[i] = 0.0f;
		pose.weightsTracks[i] = 255;
	}
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FUR_COLOR_WHITE;
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
	
	for(uint32_t i=0; i<pose.numXforms; ++i)
	{
		fm_xform_identity(&pose.xforms[i]);
		pose.weightsXforms[i] = 255;
	}
	
	for(uint32_t i=0; i<pose.numTracks; ++i)
	{
		pose.tracks[i] = 0.0f;
		pose.weightsTracks[i] = 255;
	}
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FUR_COLOR_WHITE;
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
} fa_cmd_anim_sample_data_t;

fa_cmd_status_t fa_cmd_impl_anim_sample(fa_cmd_context_t* ctx, const void* cmdData)
{
	const fa_cmd_anim_sample_data_t* data = (fa_cmd_anim_sample_data_t*)cmdData;
	FUR_ASSERT(data->animClipId < ctx->numAnimClips);
	
	const fa_anim_clip_t* clip = ctx->animClips[data->animClipId];
	
	fa_pose_stack_push(ctx->poseStack, 1);
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	// temporary ref pose write - todo: remove that, this should be part of anim clip sampling function
	{
		FUR_ASSERT(pose.numXforms == ctx->rig->numBones);
		
		for(uint32_t i=0; i<pose.numXforms; ++i)
		{
			pose.xforms[i] = ctx->rig->refPose[i];
			pose.weightsXforms[i] = 255;
		}
		
		for(uint32_t i=0; i<pose.numTracks; ++i)
		{
			pose.tracks[i] = 0.0f;
			pose.weightsTracks[i] = 255;
		}
	}
	
	fa_anim_clip_sample(clip, data->time, &pose);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FUR_COLOR_WHITE;
		char txt[256];
		sprintf(txt, "anim_sample %s t=%1.2f", fc_string_hash_as_cstr_debug(clip->name), data->time);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_anim_sample(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId)
{
	fa_cmd_anim_sample_data_t data = { time, animClipId };
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
		const float color[4] = FUR_COLOR_WHITE;
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
		const float color[4] = FUR_COLOR_WHITE;
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

void fa_action_reset(fa_action_t* action)
{
	memset(action, 0, sizeof(fa_action_t));
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
	
	fa_character_layer_t layerEnum = FA_CHAR_LAYER_BODY;
	fa_layer_t* layer = &character->layers[layerEnum];
	
	// resolve scheduled actions
	{
		const bool currInProgress = layer->currAction.userData != NULL;
		const bool nextInProgress = layer->nextAction.userData != NULL;
		const bool scheduledA = layer->scheduledActions[0].userData != NULL;
		const bool scheduledB = layer->scheduledActions[1].userData != NULL;
		if(scheduledA && scheduledB)
		{
			// cache pose
			if(currInProgress && nextInProgress)
			{
				if(layer->scheduledActions[0].args.fadeInSec <= 0.0f)
				{
					layer->currAction = layer->scheduledActions[0];
					fa_action_reset(&layer->scheduledActions[0]);
				}
				
				if(layer->scheduledActions[1].args.fadeInSec <= 0.0f)
				{
					layer->currAction = layer->scheduledActions[1];	// assigning to curr, not next, just like above
					fa_action_reset(&layer->scheduledActions[1]);
					fa_action_reset(&layer->nextAction);
				}
			}
			else
			{
				layer->currAction = layer->scheduledActions[0];
				fa_action_reset(&layer->scheduledActions[0]);
				layer->nextAction = layer->scheduledActions[1];
				fa_action_reset(&layer->scheduledActions[1]);
			}
		}
	}
	
	// if at this point we still have pending scheduled actions, then it means we need to cache the pose of current actions
	const bool stillScheduledA = layer->scheduledActions[0].userData != NULL;
	const bool stillScheduledB = layer->scheduledActions[1].userData != NULL;
	
	const bool isCachingPose = stillScheduledA || stillScheduledB;
	
	fa_cmd_context_debug_t debug = {};
	
	// animate action
	{
		fa_cmd_buffer_t animCmdBuffer = { animCmdBufferMemory, animCmdBufferSize };
		fa_cmd_buffer_recorder_t recorder = {};
		fa_cmd_buffer_recorder_init(&recorder, animCmdBuffer.data, animCmdBuffer.size);
		
		fa_action_t* action = &layer->currAction;
		
		// record commands
		FUR_ASSERT(layer->currAction.func != NULL);
		FUR_ASSERT(layer->currAction.getAnimsFunc != NULL);
		
		float localTime = -1.0f;
		if(ctx->globalTime >= action->globalStartTime)
		{
			localTime = (float)((ctx->globalTime - action->globalStartTime) / 1000000.0);
		}
		
		fa_action_ctx_t actionCtx = {};
		actionCtx.dt = ctx->dt;
		actionCtx.layer = layerEnum;
		actionCtx.cmdRecorder = &recorder;
		actionCtx.localTime = localTime;
		
		const fa_action_args_t* args = &action->args;
		
		fa_cmd_begin(&recorder, 0);
		
		if(localTime != -1.0f)
		{
			if(args->fadeInSec > 0.0f)
			{
				float alpha = fm_clamp(localTime / args->fadeInSec, 0.0f, 1.0f);
				if(args->fadeInCurve == FA_CURVE_UNIFORM_S)
				{
					alpha = fm_curve_uniform_s(alpha);
				}
				
				if(alpha < 1.0f)
				{
					if(character->transitionPoseCached)
					{
						fa_cmd_use_cached_pose(&recorder, 0); // todo: set proper index in the future
					}
					else
					{
						fa_cmd_ref_pose(&recorder);
					}
					layer->currAction.func(&actionCtx, action->userData);
					fa_cmd_blend2(&recorder, alpha);
				}
				else
				{
					layer->currAction.func(&actionCtx, action->userData);
				}
			}
			else
			{
				layer->currAction.func(&actionCtx, action->userData);
			}
		}
		else
		{
			fa_cmd_ref_pose(&recorder);
		}
		
		fa_cmd_end(&recorder);
		
		// evaluate commands
		fa_cmd_context_t animCtx = {};
		animCtx.animClips = action->getAnimsFunc(action->userData, &animCtx.numAnimClips);
		animCtx.rig = character->rig;
		animCtx.poseStack = &poseStack;
		animCtx.poseCache = &character->poseCache;
		animCtx.debug = &debug;
		
		fa_cmd_buffer_evaluate(&animCmdBuffer, &animCtx);
	}
	
	// optionally cache pose
	if(isCachingPose && stillScheduledA && !stillScheduledB)
	{
		fa_pose_t outPose;
		fa_pose_stack_get(&poseStack, &outPose, 0);
		
		fa_pose_copy(&character->poseCache.tempPose, &outPose);
		character->transitionPoseCached = true;
		const float color[4] = FUR_COLOR_RED;
		fc_dbg_text(-450.0f, 1.0f, "caching_pose", color);
	}
	
	// animate next action
	{
		fa_character_layer_t layerEnum = FA_CHAR_LAYER_BODY;
		
		fa_cmd_buffer_t animCmdBuffer = { animCmdBufferMemory, animCmdBufferSize };
		fa_cmd_buffer_recorder_t recorder = {};
		fa_cmd_buffer_recorder_init(&recorder, animCmdBuffer.data, animCmdBuffer.size);
		
		fa_layer_t* layer = &character->layers[layerEnum];
		fa_action_t* nextAction = &layer->nextAction;
		
		if(nextAction->userData != NULL)
		{
			// record commands
			FUR_ASSERT(nextAction->func != NULL);
			FUR_ASSERT(nextAction->getAnimsFunc != NULL);
			
			float localTime = -1.0f;
			if(ctx->globalTime >= nextAction->globalStartTime)
			{
				localTime = (float)((ctx->globalTime - nextAction->globalStartTime) / 1000000.0);
			}
			
			fa_action_ctx_t actionCtx = {};
			actionCtx.dt = ctx->dt;
			actionCtx.layer = layerEnum;
			actionCtx.cmdRecorder = &recorder;
			actionCtx.localTime = localTime;
			
			const fa_action_args_t* args = &nextAction->args;
			
			FUR_ASSERT(poseStack.bufferSize > 0);	// we need at least one pose on stack to blend with
			
			bool recorded = false;
			bool fullyBlended = false;
			
			if(localTime != -1.0f)
			{
				if(args->fadeInSec > 0.0f)
				{
					float alpha = fm_clamp(localTime / args->fadeInSec, 0.0f, 1.0f);
					if(args->fadeInCurve == FA_CURVE_UNIFORM_S)
					{
						alpha = fm_curve_uniform_s(alpha);
					}
					
					fa_cmd_begin(&recorder, poseStack.numPoses);
					layer->nextAction.func(&actionCtx, nextAction->userData);
					fa_cmd_blend2(&recorder, alpha);
					fa_cmd_end(&recorder);
					recorded = true;
					
					if(alpha >= 1.0f)
					{
						fullyBlended = true;
					}
				}
				else
				{
					fa_cmd_begin(&recorder, poseStack.numPoses);
					layer->nextAction.func(&actionCtx, nextAction->userData);
					fa_cmd_blend2(&recorder, 1.0);
					fa_cmd_end(&recorder);
					recorded = true;
					fullyBlended = true;
				}
			}
			
			if(recorded)
			{
				// evaluate commands
				fa_cmd_context_t animCtx = {};
				animCtx.animClips = nextAction->getAnimsFunc(nextAction->userData, &animCtx.numAnimClips);
				animCtx.rig = character->rig;
				animCtx.poseStack = &poseStack;
				animCtx.poseCache = &character->poseCache;
				animCtx.debug = &debug;
				
				fa_cmd_buffer_evaluate(&animCmdBuffer, &animCtx);
				
				if(fullyBlended)
				{
					layer->currAction = layer->nextAction;
					fa_action_reset(&layer->nextAction);
					character->transitionPoseCached = false;
				}
			}
		}
	}
	
	// out pose is the result of this layer
	fa_pose_t outPose;
	fa_pose_stack_get(&poseStack, &outPose, 0);
	
	// optionally cache pose
	if(isCachingPose && stillScheduledA && stillScheduledB)
	{
		fa_pose_copy(&character->poseCache.tempPose, &outPose);
		character->transitionPoseCached = true;
		
		layer->currAction = layer->scheduledActions[0];
		fa_action_reset(&layer->scheduledActions[0]);
		layer->nextAction = layer->scheduledActions[1];
		fa_action_reset(&layer->scheduledActions[1]);
	}
	
	if(stillScheduledA && !stillScheduledB)
	{
		FUR_ASSERT(!stillScheduledB);
		layer->currAction = layer->nextAction;
		fa_action_reset(&layer->nextAction);
		layer->nextAction = layer->scheduledActions[0];
		fa_action_reset(&layer->scheduledActions[0]);
	}
	
	// convert to model space
	{
		const int16_t* parentIndices = character->rig->parents;
		fa_pose_t poseMS = {};
		poseMS.xforms = character->poseMS;
		poseMS.numXforms = character->rig->numBones;
		fa_pose_local_to_model(&poseMS, &outPose, parentIndices);
	}
}

void fa_action_animate_func(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_animate_t* data = (fa_action_animate_t*)userData;
	
	const float animDuration = data->animation->duration;
	const float time = fmodf(ctx->localTime, animDuration);
	fa_cmd_anim_sample(ctx->cmdRecorder, time, 0);
}

const fa_anim_clip_t** fa_action_animate_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_animate_t* data = (const fa_action_animate_t*)userData;
	*numAnims = 1;
	return (const fa_anim_clip_t**)&data->animation;	// todo: check it, is this return correct?
}

void fa_character_schedule_action_simple(fa_character_t* character, fa_action_animate_t* action, const fa_action_args_t* args, uint64_t currGlobalTime)
{
	fa_layer_t* layer = &character->layers[FA_CHAR_LAYER_BODY];
	
	fa_action_t* actionSlot = NULL;
	
	if(layer->currAction.userData == NULL)
	{
		actionSlot = &layer->currAction;
	}
	else if(layer->nextAction.userData == NULL)
	{
		actionSlot = &layer->nextAction;
	}
	else if(layer->scheduledActions[0].userData == NULL)
	{
		actionSlot = &layer->scheduledActions[0];
	}
	else if(layer->scheduledActions[1].userData == NULL)
	{
		actionSlot = &layer->scheduledActions[1];
	}
	else
	{
		layer->scheduledActions[0] = layer->scheduledActions[1];
		fa_action_reset(&layer->scheduledActions[1]);
		actionSlot = &layer->scheduledActions[1];
	}
	
	actionSlot->userData = action;
	actionSlot->func = fa_action_animate_func;
	actionSlot->getAnimsFunc = fa_action_animate_get_anims_func;
	actionSlot->globalStartTime = currGlobalTime;
	actionSlot->args = *args;
}

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
	
	const float inv_dt = (dt > 0.00000001f) ? 1.0f / dt : 0.0f;
	
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
		}
	}
	
	for(uint32_t i=1; i<count; ++i)	// start from 1, as 0 is attach point
	{
		fm_vec4_sub(&dangle->p[i], &dangle->x0[i], &dangle->v[i]);
		fm_vec4_mulf(&dangle->v[i], inv_dt, &dangle->v[i]);
		dangle->x0[i] = dangle->p[i];
	}
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
