/* Copyright (c) 2016-2022 Furball Cat */

#include "pose.h"
#include "rig.h"
#include "ccore/public.h"
#include "cmath/public.h"
#include <string.h>

void fa_pose_set_identity(fa_pose_t* pose, const u8* mask)
{
	fm_xform* xforms = pose->xforms;
	f32* tracks = pose->tracks;
	
	const u8* weightsXformsBegin = pose->weightsXforms;
	const u8* weightsTracksBegin = pose->weightsTracks;
	
	const u8* weightsXforms = weightsXformsBegin;
	const u8* weightsTracks = weightsTracksBegin;
	
	for(u32 i=0; i<pose->numXforms; ++i)
	{
		const u8 maskValue = mask ? mask[i] : 255;
		const u8 weight = weightsXformsBegin ? *weightsXforms : maskValue;
		
		if(weight != 0)
		{
			fm_xform_identity(xforms);
		}
		
		xforms++;
		weightsXforms++;
	}
	
	for(u32 i=0; i<pose->numTracks; ++i)
	{
		const u8 weight = weightsTracksBegin ? *weightsTracks : 255;
		
		if(weight != 0)
		{
			*tracks = 0.0f;
		}
		
		tracks++;
		weightsTracks++;
	}
}

void fa_pose_set_reference(const fa_rig_t* rig, fa_pose_t* pose, const u8* mask)
{
	FUR_ASSERT(rig->numBones == pose->numXforms);
	
	const fm_xform* refXForms = rig->refPose;
	fm_xform* xforms = pose->xforms;
	f32* tracks = pose->tracks;
	
	const u8* weightsXformsBegin = pose->weightsXforms;
	const u8* weightsTracksBegin = pose->weightsTracks;
	
	const u8* weightsXforms = weightsXformsBegin;
	const u8* weightsTracks = weightsTracksBegin;
	
	for(u32 i=0; i<pose->numXforms; ++i)
	{
		const u8 maskValue = mask ? mask[i] : 255;
		const u8 weight = weightsXformsBegin ? *weightsXforms : maskValue;
		
		if(weight != 0)
		{
			*xforms = *refXForms;
		}
		
		xforms++;
		weightsXforms++;
		refXForms++;
	}
	
	for(u32 i=0; i<pose->numTracks; ++i)
	{
		const u8 weight = weightsTracksBegin ? *weightsTracks : 255;
		
		if(weight != 0)
		{
			*tracks = 0.0f;
		}
		
		tracks++;
		weightsTracks++;
	}
}

// -----

void fa_pose_copy(fa_pose_t* dest, const fa_pose_t* src)
{
	const u32 numXforms = FM_MIN(src->numXforms, dest->numXforms);
	if(numXforms > 0)
	{
		memcpy(dest->xforms, src->xforms, sizeof(fm_xform) * numXforms);
		if(dest->weightsXforms && src->weightsXforms)
		{
			memcpy(dest->weightsXforms, src->weightsXforms, sizeof(u8) * numXforms);
		}
	}
	
	const u32 numTracks = FM_MIN(src->numTracks, dest->numTracks);
	if(numTracks > 0)
	{
		memcpy(dest->tracks, src->tracks, sizeof(f32) * numTracks);
		if(dest->weightsTracks && src->weightsTracks)
		{
			memcpy(dest->weightsTracks, src->weightsTracks, sizeof(u8) * numXforms);
		}
	}
}

void fa_pose_local_to_model(fa_pose_t* modelPose, const fa_pose_t* localPose, const int16_t* parentIndices)
{
	const fm_xform* localXforms = localPose->xforms;
	fm_xform* modelXforms = modelPose->xforms;
	
	u32 numBones = FM_MIN(modelPose->numXforms, localPose->numXforms);
	
	// iterate non-root bones
	for(u16 i = 0; i < numBones; ++i)
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
	u32 numTracks = FM_MIN(modelPose->numTracks, localPose->numTracks);
	if(numTracks > 0)
	{
		memcpy(modelPose->tracks, localPose->tracks, sizeof(f32) * numTracks);
	}
}

void fa_pose_blend_linear(fa_pose_t* out, const fa_pose_t* b, const fa_pose_t* a, f32 alpha)
{
	FUR_ASSERT(out->numXforms == a->numXforms && a->numXforms == b->numXforms);
	FUR_ASSERT(out->numTracks == a->numTracks && a->numTracks == b->numTracks);
	FUR_ASSERT((out->weightsXforms || out->numXforms == 0) && (out->weightsTracks || out->numTracks == 0));
	
	// blend xforms
	{
		const u32 numXforms = out->numXforms;
		const fm_xform* a_xforms = a->xforms;
		const fm_xform* b_xforms = b->xforms;
		fm_xform* out_xforms = out->xforms;

		const u8* const a_weightsBase = a->weightsXforms;
		const u8* const b_weightsBase = b->weightsXforms;
		
		const u8* a_weights = a_weightsBase;
		const u8* b_weights = b_weightsBase;
		u8* out_weights = out->weightsXforms;

		for(u32 i=0; i<numXforms; ++i)
		{
			const u8 a_byte = a_weightsBase ? *a_weights : 255;
			const u8 b_byte = b_weightsBase ? *b_weights : 255;
			
			const bool a_valid = (a_byte != 0);
			const bool b_valid = (b_byte != 0);
			
			if(a_valid && b_valid)
			{
				const f32 a_weight = a_byte * (1.0f / 255.0f);
				const f32 b_weight = b_byte * (1.0f / 255.0f);
				
				const f32 blendFactor = (b_weight > a_weight) ? ((b_weight - a_weight + alpha * a_weight) / b_weight) : alpha * b_weight / a_weight;
				const f32 outWeight = (1.0f - blendFactor) * a_weight + blendFactor * b_weight;
				
				fm_xform_slerp(a_xforms, b_xforms, blendFactor, out_xforms);
				
				*out_weights = (u8)(outWeight * 255.0f + 0.5f);
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
		const u32 numTracks = out->numTracks;
		const f32* a_tracks = a->tracks;
		const f32* b_tracks = b->tracks;
		f32* out_tracks = out->tracks;
		
		const u8* const a_weightsBase = a->weightsTracks;
		const u8* const b_weightsBase = b->weightsTracks;
		
		const u8* a_weights = a_weightsBase;
		const u8* b_weights = b_weightsBase;
		u8* out_weights = out->weightsTracks;
		
		for(u32 i=0; i<numTracks; ++i)
		{
			const u8 a_byte = a_weightsBase ? *a_weights : 255;
			const u8 b_byte = b_weightsBase ? *b_weights : 255;
			
			const bool a_valid = (a_byte != 0);
			const bool b_valid = (b_byte != 0);
			
			if(a_valid && b_valid)
			{
				const f32 a_weight = a_byte * (1.0f / 255.0f);
				const f32 b_weight = b_byte * (1.0f / 255.0f);
				
				const f32 blendFactor = (b_weight > a_weight) ? ((b_weight - a_weight + alpha * a_weight) / b_weight) : alpha * b_weight / a_weight;
				const f32 outWeight = (1.0f - blendFactor) * a_weight + blendFactor * b_weight;
				
				*out_tracks = (1.0f - blendFactor) * (*a_tracks) + blendFactor * (*b_tracks);
				
				*out_weights = (u8)(outWeight * 255.0f + 0.5f);
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

void fa_pose_apply_additive(fa_pose_t* out, const fa_pose_t* base, const fa_pose_t* add, f32 weight)
{
	// blend xforms
	{
		const u32 numXforms = out->numXforms;
		const fm_xform* base_xforms = base->xforms;
		const fm_xform* add_xforms = add->xforms;
		fm_xform* out_xforms = out->xforms;
		
		if(weight == 1.0f)
		{
			for(u32 i=0; i<numXforms; ++i)
			{
				fm_xform_mul(&base_xforms[i], &add_xforms[i], &out_xforms[i]);
			}
		}
		else
		{
			for(u32 i=0; i<numXforms; ++i)
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
		const u32 numTracks = out->numTracks;
		const f32* base_tracks = base->tracks;
		const f32* add_tracks = add->tracks;
		f32* out_tracks = out->tracks;
		
		for(u32 i=0; i<numTracks; ++i)
		{
			out_tracks[i] = base_tracks[i] + add_tracks[i] * weight;
		}
	}
}
