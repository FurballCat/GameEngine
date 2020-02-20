/* Copyright (c) 2016-2019 Furball Cat */

#include "import.h"
#include <string.h>
#include "import/public.h"		// todo: move that to toolset, then load through resource system
#include "ccore/public.h"

bool IsFileExtensionEqualTo(const char* path, const char* ext)
{
	const size_t pathLen = strlen(path);
	const size_t extLen = strlen(ext);
	
	if(strcmp(path + pathLen - extLen, ext) == 0)
	{
		return true;
	}
	
	return false;
}

ofbx::IScene* OpenScene_FBX(const char* path)
{
	FileStream file(path, FileStream::in | FileStream::binary | FileStream::ate);
	FUR_ASSERT(file);
	
	uint32_t fileSize = (uint32)file.tellg();
	file.seekg(0, FileStream::beg);
	
	DynArray<char> fileData(fileSize);
	
	file.read(&fileData[0], fileSize);
	file.close();
	
	FUR_ASSERT(!fileData.empty());
	
	ofbx::IScene* scene = ofbx::load((const uint8*)fileData.data(), (int)fileData.size());
	FUR_ASSERT(scene);
	
	return scene;
}

fi_result_t fi_import_mesh(const fi_depot_t* depot, const fi_import_mesh_ctx_t* ctx, fr_resource_mesh_t** ppMesh, fc_alloc_callbacks_t* pAllocCallbacks)
{
	std::string absolutePath = depot->path;
	absolutePath += ctx->path;
	
	// todo: validate absolute path
	
	if(IsFileExtensionEqualTo(absolutePath.c_str(), ".fbx"))
	{
		ofbx::IScene* scene = OpenScene_FBX(absolutePath.c_str());
		
		FUR_ASSERT(scene);
		
		const int32_t numMeshes = scene->getMeshCount();
		
		fr_resource_mesh_t* mesh = (fr_resource_mesh_t*)FUR_ALLOC_AND_ZERO(sizeof(fr_resource_mesh_t), 0, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		mesh->chunks = (fr_resource_mesh_chunk_t*)FUR_ALLOC_AND_ZERO(sizeof(fr_resource_mesh_chunk_t) * numMeshes, 0, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		
		for(int32_t i=0; i<numMeshes; ++i)
		{
			fr_resource_mesh_chunk_t* chunk = &mesh->chunks[i];
			
			const ofbx::Mesh* mesh = scene->getMesh(i);
			const ofbx::Geometry* geometry = mesh->getGeometry();
			
			const int32 numVertices = geometry->getVertexCount();
			const ofbx::Vec3* vertices = geometry->getVertices();
			const ofbx::Vec3* normals = geometry->getNormals();
			const ofbx::Vec2* uvs = geometry->getUVs();
			
			//rend::SimpleVertexLayout vertex;
			
			const uint32_t sizeVec3 = sizeof(float) * 3;
			const uint32_t sizeVec2 = sizeof(float) * 2;
			const uint32_t stride = sizeVec3 + sizeVec3 + sizeVec2;
			
			chunk->dataVertices = (float*)FUR_ALLOC(stride * numVertices, 16, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
			chunk->numVertices = numVertices;
			chunk->vertexStride = stride;
			
			chunk->numIndices = numVertices;
			chunk->dataIndices = (uint16_t*)FUR_ALLOC(sizeof(uint16_t) * numVertices, 16, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
			
			float* itVertex = chunk->dataVertices;
			uint16_t* itIndex = chunk->dataIndices;
			
			for(int32 iv=0; iv<numVertices; ++iv)
			{
				float* position = itVertex;
				float* normal = itVertex + 3;
				float* uv = itVertex + 3;
				
				position[0] = vertices[iv].x;
				position[1] = vertices[iv].y;
				position[2] = vertices[iv].z;
				
				normal[0] = normals[iv].x;
				normal[1] = normals[iv].y;
				normal[2] = normals[iv].z;
				
				uv[0] = uvs[iv].x;
				uv[1] = uvs[iv].y;
				
				*itIndex = (uint16_t)iv;
				
				itVertex += stride;
				itIndex += 1;
			}
			
			FUR_ASSERT(itVertex == chunk->dataVertices + stride * numVertices);
			FUR_ASSERT(itIndex == chunk->dataIndices + numVertices);
		}
		
		return FI_RESULT_OK;
	}
	
	return FI_RESULT_UNKNOWN_FILE_FORMAT_IMPORT_ERROR;
}

void fr_release_mesh(fr_resource_mesh_t** ppMesh, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fr_resource_mesh_chunk_t* chunks = (*ppMesh)->chunks;
	uint32_t numChunks = (*ppMesh)->numChunks;
	
	for(uint32_t i=0; i<numChunks; ++i)
	{
		FUR_FREE(chunks->dataIndices, pAllocCallbacks);
		FUR_FREE(chunks->dataVertices, pAllocCallbacks);
	}
	
	FUR_FREE((*ppMesh)->chunks, pAllocCallbacks);
	FUR_FREE(*ppMesh, pAllocCallbacks);
	*ppMesh = NULL;
}
