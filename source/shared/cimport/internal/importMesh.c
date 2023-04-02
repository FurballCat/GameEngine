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

void fcMeshResourceSerialize(FcSerializer* pSerializer, FcMeshResource* mesh, FcAllocator* pAllocCallbacks)
{
	FUR_SER_VERSION(FR_MESH_VER_LAST-1);
	
	FUR_SER_ADD(FR_MESH_VER_BASE, mesh->version);
	FUR_SER_ADD(FR_MESH_VER_BASE, mesh->numChunks);
	
	if(!pSerializer->isWriting)
	{
		FUR_ASSERT(mesh->chunks == NULL);
		mesh->chunks = FUR_ALLOC_ARRAY_AND_ZERO(FcMeshResourceChunk, mesh->numChunks, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
	}
	
	for(i32 i=0; i<mesh->numChunks; ++i)
	{
		FcMeshResourceChunk* chunk = &mesh->chunks[i];
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numBones);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numIndices);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->numVertices);
		FUR_SER_ADD(FR_MESH_VER_BASE, chunk->vertexStride);
		
		if(!pSerializer->isWriting)
		{
			chunk->dataVertices = (f32*)FUR_ALLOC_AND_ZERO(chunk->vertexStride * sizeof(f32) * chunk->numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			chunk->dataIndices = FUR_ALLOC_ARRAY_AND_ZERO(u32, chunk->numIndices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		}
		
		// geometry data
		FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataVertices, chunk->vertexStride * sizeof(f32) * chunk->numVertices);
		FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataIndices, sizeof(u32) * chunk->numIndices);
		
		// skinning data
		if(chunk->numBones > 0)
		{
			if(!pSerializer->isWriting)
			{
				chunk->bindPose = FUR_ALLOC_ARRAY_AND_ZERO(fm_mat4, chunk->numBones, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				chunk->boneNameHashes = FUR_ALLOC_ARRAY_AND_ZERO(FcStringId, chunk->numBones, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
				chunk->dataSkinning = FUR_ALLOC_ARRAY_AND_ZERO(FcMeshResourceChunkSkin, chunk->numVertices, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			}
			
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->bindPose, sizeof(fm_mat4) * chunk->numBones);
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->boneNameHashes, sizeof(FcStringId) * chunk->numBones);
			FUR_SER_ADD_BUFFER(FR_MESH_VER_BASE, chunk->dataSkinning, sizeof(FcMeshResourceChunkSkin) * chunk->numVertices);
		}
	}
}
