/* Copyright (c) Furball Cat */

#include "import.h"
#include "ccore/public.h"
#include "ccore/serialize.h"
#include "cmath/public.h"

typedef enum fr_mesh_version_t
{
	FR_MESH_VER_BASE = 0,
	FR_MESH_VER_LAST,
} fa_anim_clip_version_t;

void fr_resource_mesh_serialize(fc_serializer_t* pSerializer, fr_resource_mesh_t* mesh, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_SER_VERSION(FR_MESH_VER_LAST-1);
	
	FUR_SER_ADD(FR_MESH_VER_BASE, mesh->version);
	FUR_SER_ADD(FR_MESH_VER_BASE, mesh->numChunks);
	
	if(!pSerializer->isWriting)
	{
		FUR_ASSERT(mesh->chunks == NULL);
		mesh->chunks = FUR_ALLOC_ARRAY_AND_ZERO(fr_resource_mesh_chunk_t, mesh->numChunks, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
	}
	
	for(int32_t i=0; i<mesh->numChunks; ++i)
	{
		fr_resource_mesh_chunk_t* chunk = &mesh->chunks[i];
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numBones);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numIndices);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numVertices);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->vertexStride);
		
		if(!pSerializer->isWriting)
		{
			chunk->dataVertices = (float*)FUR_ALLOC_AND_ZERO(chunk->vertexStride * sizeof(float) * chunk->numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			chunk->dataIndices = FUR_ALLOC_ARRAY_AND_ZERO(uint32_t, chunk->numIndices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		}
		
		// geometry data
		FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataVertices, chunk->vertexStride * sizeof(float) * chunk->numVertices);
		FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataIndices, sizeof(uint32_t) * chunk->numIndices);
		
		// skinning data
		if(chunk->numBones > 0)
		{
			if(!pSerializer->isWriting)
			{
				chunk->bindPose = FUR_ALLOC_ARRAY_AND_ZERO(fm_mat4, chunk->numBones, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				chunk->boneNameHashes = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, chunk->numBones, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				chunk->dataSkinning = FUR_ALLOC_ARRAY_AND_ZERO(fr_resource_mesh_chunk_skin_t, chunk->numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			}
			
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->bindPose, sizeof(fm_mat4) * chunk->numBones);
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->boneNameHashes, sizeof(fc_string_hash_t) * chunk->numBones);
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataSkinning, sizeof(fr_resource_mesh_chunk_skin_t) * chunk->numVertices);
		}
	}
}
