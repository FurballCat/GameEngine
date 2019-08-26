#pragma once

namespace utils
{
	struct VertexData
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
	
	enum class VertexLayoutAttribute : uint8
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
	
	struct VertexBufferBuilderContext
	{
		void* const m_vertices;
		
		const VertexLayoutAttribute* const m_attributes;
		const uint32 m_vertexBufferSize;
		const uint32 m_numAttributes;
		const uint32 m_stride;
		
		uint32 m_idx;
	};
	
	WORLD_API void AddVertex(VertexBufferBuilderContext& ctx, const VertexData& vertex);
}
