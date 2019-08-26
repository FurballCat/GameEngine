#pragma once

namespace gpu
{
	class Buffer;
	class Device;
	struct VertexDescriptor;
}

namespace rend
{
	struct SimpleVertexLayout
	{
		math::Vector3 m_position;
		math::Vector3 m_color;
		math::Vector3 m_normal;
		math::Vector3 m_tangent;
		math::Vector3 m_binormal;
		math::Vector2 m_uv;
		math::UintVector4 m_skinJointsA;
		math::UintVector4 m_skinJointsB;
		math::Vector4 m_skinWeightsA;
		math::Vector4 m_skinWeightsB;
	};
	
	enum class VertexLayoutAttribute
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
	
	class RENDERER_API GeometryBuilder
	{
	public:
		void ReserveMemory(uint32 numVertices);
		void AddVertex(const SimpleVertexLayout& layout);
		void AddIndex(uint32 index);
		void BuildGeometryBuffer(gpu::Device* device, const VertexLayoutAttribute* attributes, uint32 numAttributes, uint32 bufferIndex,
								 UniquePtr<gpu::Buffer>& outBuffer, gpu::VertexDescriptor* outVertexDescription) const;
		void BuildGeometryBuffer(gpu::Device* device, const VertexLayoutAttribute* attributes, uint32 numAttributes, uint32 bufferIndex,
								 gpu::Buffer& outBuffer, gpu::VertexDescriptor* outVertexDescription) const;
		
		void BuildIndexBuffer(gpu::Device* device, UniquePtr<gpu::Buffer>& outBuffer) const;
		void BuildIndexBuffer(gpu::Device* device, gpu::Buffer& outBuffer) const;
		
		uint32 NumIndices() const { return (uint32)m_indices.size(); }
		uint32 NumVertices() const { return (uint32)m_vertices.size(); }
		
	private:
		// current builder settings, per vertex
		math::Vector4 m_normal;
		
		DynArray<SimpleVertexLayout> m_vertices;
		DynArray<uint32> m_indices;
	};
	
	struct MeshChunk;
	RENDERER_API void BuildMeshChunk(const GeometryBuilder& builder, gpu::Device* device, const VertexLayoutAttribute* attributes, uint32 numAttributes,
									 uint32 bufferIndex, rend::MeshChunk& outMeshChunk);
}
