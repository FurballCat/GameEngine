#pragma once

namespace world
{
	// consoles IO limit: 30 mb/s
	struct Mesh
	{
		
	};
	
	struct WORLD_API ObjectPalette
	{
		Mesh* m_meshes;
		uint32 m_numMeshes;
		// uint32 m_numTextures;
		// Texture* m_textures;
		// Particles* m_particles;
		// Animation* m_animations;
		// SpriteLibrary* m_spriteLibraries;
		// Spawner* m_spawners;	// entities and components baked into spawners, this has characters, exploding barrels and stuff,
	};
	
	// todo: this is rather an offline code, for creating and saving object palettes
	enum AssetLoaderType : uint32
	{
		ASSET_LOADER_UNKNOWN = 0,
		
		// mesh loaders
		ASSET_MESH_TYPE_BEGIN,
		
		ASSET_LOADER_MESH_OBJ,
		
		ASSET_MESH_TYPE_END,
	};
	
	struct AssetEntry
	{
		String m_path;
		AssetLoaderType m_loaderType;
	};
	
	struct WORLD_API ObjectPaletteDesc
	{
		DynArray<AssetEntry> m_meshes;
		// DynArray<AssetEntry> m_textures;
	};
	
	AssetLoaderType GetAssetLoaderType(const String& file);
	bool IsMeshAsset(AssetLoaderType loaderType);
	bool IsUnknownAsset(AssetLoaderType loaderType);
	
	WORLD_API bool CreateObjectPaletteDesc(ArrayView<const String> files, ObjectPaletteDesc& outDesc);
	
	bool LoadMesh_OBJ(StringView path, AssetLoaderType loaderType, Mesh* const mesh);
	bool LoadMesh(StringView path, AssetLoaderType loaderType, Mesh* const mesh);
	bool LoadAllMeshes(const AssetEntry* const assets, Mesh* const meshes, uint32 count);
	
	WORLD_API ObjectPalette* LoadObjectPalette(FileStream& file);
	WORLD_API bool BakeObjectPaletteToPackage(const ObjectPaletteDesc& desc, FileStream& file);
}
