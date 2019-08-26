#include "pch.h"
#include "geometryBuilder.h"

using namespace utils;

void utils::AddVertex(VertexBufferBuilderContext& ctx, const VertexData& vertex)
{
	const uint32 pos = ctx.m_idx * ctx.m_stride;
	FUR_ASSERT(pos < ctx.m_vertexBufferSize);
	//uint8* vertexPtr = &ctx.m_vertices[pos];
	
	for(uint32 i=0; i<ctx.m_numAttributes; ++i)
	{
		
	}
}
