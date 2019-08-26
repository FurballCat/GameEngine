#include "pch.h"
#include "objectPalette.h"

namespace
{
	bool EndsWith(const String& str, const char* ending)
	{
		const uint32 ending_len = (uint32)std::strlen(ending);
		const uint32 str_len = (uint32)str.size();
		
		if(str_len < ending_len)
			return false;
		
		const uint32 str_idx = str_len - ending_len;
		
		for(uint32 i=0; i<ending_len; ++i)
		{
			if(str[str_idx + i] != ending[i])
				return false;
		}
		
		return true;
	}
}

namespace world
{
	AssetLoaderType GetAssetLoaderTypeByExtension(const String& file)
	{
		if(EndsWith(file, ".obj"))
			return ASSET_LOADER_MESH_OBJ;
		
		return ASSET_LOADER_UNKNOWN;
	}
	
	bool IsMeshAsset(AssetLoaderType loaderType)
	{
		return ASSET_MESH_TYPE_BEGIN < loaderType && loaderType < ASSET_MESH_TYPE_END;
	}
	
	bool IsUnknownAsset(AssetLoaderType loaderType)
	{
		return loaderType == ASSET_LOADER_UNKNOWN;
	}
	
	bool CreateObjectPaletteDesc(ArrayView<const String> files, ObjectPaletteDesc& outDesc)
	{
		const uint32 numSourceFiles = (uint32)files.Size();
		
		for(uint32 i=0; i<numSourceFiles; ++i)
		{
			AssetLoaderType loaderType = GetAssetLoaderTypeByExtension(files[i]);
			if(IsMeshAsset(loaderType))
				outDesc.m_meshes.push_back({files[i], loaderType});
			else if(IsUnknownAsset(loaderType))
				return false;
		}
		
		return true;
	}
	
	struct MeshData
	{
		mem::BinaryBuffer m_vertices;
		mem::BinaryBuffer m_indices;
	};
	
	bool LoadMesh_OBJ(StringView path, AssetLoaderType loaderType, Mesh* const mesh)
	{
		return false;
	}
	
	bool LoadMesh(StringView path, AssetLoaderType loaderType, Mesh* const mesh)
	{
		switch(loaderType)
		{
			case ASSET_LOADER_MESH_OBJ:
				return LoadMesh_OBJ(path, loaderType, mesh);
			default:
				return false;
		}
		
		return false;
	}
	
	bool LoadAllMeshes(const AssetEntry* const assets, Mesh* const meshes, uint32 count)
	{
		FUR_ASSERT(meshes != nullptr);
		FUR_ASSERT(assets != nullptr);
		FUR_ASSERT(count > 0);
		
		for(uint32 i=0; i<count; ++i)
		{
			if(LoadMesh(StringView(assets[i].m_path.c_str(), assets[i].m_path.length()), assets[i].m_loaderType, &meshes[i]) == false)
				return false;
		}
		
		return true;
	}
	
	ObjectPalette* LoadObjectPalette(FileStream& file)
	{
		return nullptr;
	}
	
	bool BakeObjectPaletteToPackage(const ObjectPaletteDesc& desc, FileStream& file)
	{
		return false;
	}
}
