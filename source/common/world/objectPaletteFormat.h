#pragma once

namespace world
{
	/*
	 The Object Palette Format (OPF) is:
		OPF_Header
			OPF_SectionHeader
				<section data>
			...
	 */
	
	typedef uint64 ResourceID;	// hashed path/name of the resource, used for unique identification
	
	constexpr uint32 OPF_NEWEST_VERSION = 1000;
	
	struct OPF_Header
	{
		// version of the object palette
		uint32 m_version;
		
		// size of entire object palette
		uint32 m_size;
		
		// date of the object palette creation (number of seconds since the epoch)
		uint64 m_dateStamp;
		
		// number of all sections
		uint32 m_numSections;
		
		// required cpu memory in bytes
		uint32 m_cpuMemorySize;
		
		// required gpu memory in bytes
		uint32 m_gpuMemorySize;
	};
	
	struct OPF_SectionHeader
	{
		// section unique name/id, 4 characters, for example: 'mesh', encodes the section reader
		uint32 m_name;
		
		// size of the entire section
		uint32 m_size;
	};
	
	/*
	 The Mesh Section Format (MSF) is:
		MSF_Header
			MSF_MeshHeader
				<vertex layout data>
				MSF_MeshChunkHeader
					<vertices data>
					<indices data>
				...
			...
	 */
	
	constexpr uint32 MSF_SECTION_NAME = 'mesh';
	
	struct MSF_Header
	{
		
	};
	
	enum class MSF_VertexLayoutAttribute : uint8
	{
		Position,
		Color,
		Normal,
		Tangent,
		Binromal,
		UV,
		SkinJointsA,
		SkinJointsB,
		SkinWeightsA,
		SkinWeightsB,
	};
	
	struct MSF_MeshHeader
	{
		// this mesh resource id
		ResourceID m_meshID;
		
		// size of entire mesh data
		uint32 m_size;
		
		// size of one vertex (size of vertex type)
		uint32 m_vertexStride;
		
		// size of one index (size of index type)
		uint32 m_indexStride;
		
		// number of vertex layout attributes (position, color etc.), shared between all chunks
		uint32 m_numLayoutAttributes;
		
		// number of all mesh chunks in the mesh
		uint32 m_numMeshChunks;
		
		// mesh bounding box
		math::BBox m_boundingBox;
	};
	
	struct MSF_MeshChunkHeader
	{
		// reference to material
		ResourceID m_materialID;
		
		// size of entire mesh chunk data
		uint32 m_size;
		
		// number of all vertices in mesh chunk
		uint32 m_numVertices;
		
		// number of all indices in mesh chunk
		uint32 m_numIndices;
	};
}
