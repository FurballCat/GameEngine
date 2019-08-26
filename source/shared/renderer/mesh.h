#pragma once

namespace rend
{
	struct MeshChunk
	{
		MeshChunk() : m_vertices(), m_indices() {}
		
		gpu::Buffer m_vertices;
		gpu::Buffer m_indices;
		
		uint32 m_numVertices = 0;
		uint32 m_numIndices = 0;
	};
	
	struct Mesh
	{
		DynArray<MeshChunk> m_chunks;
	};
}
