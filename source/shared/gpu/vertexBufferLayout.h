#pragma once

namespace gpu
{
	enum class VertexFormat;
	enum class VertexStepFunction;
	
	struct VertexBufferLayoutDescriptor
	{
		uint32 m_stride;
		VertexStepFunction m_stepFunction;
		uint32 m_stepRate;
	};
	
	struct VertexAttributeDescriptor
	{
		VertexFormat m_format;
		uint32 m_offset;
		uint32 m_bufferIndex;
	};
	
    struct VertexDescriptor
	{
		// layouts of vertices
		VertexBufferLayoutDescriptor* m_layouts;
		
		// specific attributes of vertex (position, normal, etc.)
		VertexAttributeDescriptor* m_attributes;
		
		// size of layout array
		uint32 m_numLayouts;
		
		// size of attributes array
		uint32 m_numAttributes;
	};
}
