#include "pch.h"
#include "geometryBuilder.h"

using namespace rend;

namespace helper
{
	uint32 GetVertexSize(const VertexLayoutAttribute* attributes, uint32 numAttributes)
	{
		uint32 vertexSize = 0;
		
		for(uint32 i=0; i<numAttributes; ++i)
		{
			switch(attributes[i])
			{
				case VertexLayoutAttribute::Position:
				case VertexLayoutAttribute::Color:
				case VertexLayoutAttribute::Normal:
				case VertexLayoutAttribute::Tangent:
				case VertexLayoutAttribute::Binromal:
					vertexSize += sizeof(math::Vector3);
					break;
					
				case VertexLayoutAttribute::UV:
					vertexSize += sizeof(math::Vector2);
					break;
					
				case VertexLayoutAttribute::SkinJointsA:
				case VertexLayoutAttribute::SkinJointsB:
				case VertexLayoutAttribute::SkinWeightsA:
				case VertexLayoutAttribute::SkinWeightsB:
					vertexSize += sizeof(math::UintVector4);
					break;
				
				default:
					FUR_ASSERT(false);
					break;
			}
		}
		
		return vertexSize;
	}
	
	void CopyAttributeValue(float* data, uint32 dataSize, uint32& currentPosition, const SimpleVertexLayout& vertex, VertexLayoutAttribute attribute)
	{
		const uint8* source = nullptr;
		uint32 copySize = 0;
		
		switch(attribute)
		{
			case VertexLayoutAttribute::Position:
				source = reinterpret_cast<const uint8*>(&vertex.m_position);
				copySize = sizeof(vertex.m_position);
				break;
				
			case VertexLayoutAttribute::Color:
				source = reinterpret_cast<const uint8*>(&vertex.m_color);
				copySize = sizeof(vertex.m_color);
				break;
				
			case VertexLayoutAttribute::Normal:
				source = reinterpret_cast<const uint8*>(&vertex.m_normal);
				copySize = sizeof(vertex.m_normal);
				break;
				
			case VertexLayoutAttribute::Tangent:
				source = reinterpret_cast<const uint8*>(&vertex.m_tangent);
				copySize = sizeof(vertex.m_tangent);
				break;
				
			case VertexLayoutAttribute::Binromal:
				source = reinterpret_cast<const uint8*>(&vertex.m_binormal);
				copySize = sizeof(vertex.m_binormal);
				break;
				
			case VertexLayoutAttribute::UV:
				source = reinterpret_cast<const uint8*>(&vertex.m_uv);
				copySize = sizeof(vertex.m_uv);
				break;
				
			case VertexLayoutAttribute::SkinJointsA:
				source = reinterpret_cast<const uint8*>(&vertex.m_skinJointsA);
				copySize = sizeof(vertex.m_skinJointsA);
				break;
				
			case VertexLayoutAttribute::SkinJointsB:
				source = reinterpret_cast<const uint8*>(&vertex.m_skinJointsB);
				copySize = sizeof(vertex.m_skinJointsB);
				break;
				
			case VertexLayoutAttribute::SkinWeightsA:
				source = reinterpret_cast<const uint8*>(&vertex.m_skinWeightsA);
				copySize = sizeof(vertex.m_skinWeightsA);
				break;
				
			case VertexLayoutAttribute::SkinWeightsB:
				source = reinterpret_cast<const uint8*>(&vertex.m_skinWeightsB);
				copySize = sizeof(vertex.m_skinWeightsB);
				break;
				
			default:
				FUR_ASSERT(false);
				break;
		}
		
		FUR_ASSERT(dataSize > copySize);
		FUR_ASSERT(copySize > 0);
		FUR_ASSERT(currentPosition <= (dataSize - copySize));
		
		// perform copy
		uint8* destination = &reinterpret_cast<uint8*>(data)[currentPosition];
		mem::MemoryCopy(destination, source, copySize);
		currentPosition += copySize;
	}
	
	void FillVertexDescriptor(const VertexLayoutAttribute* attributes, uint32 numAttributes, uint32 bufferIndex, gpu::VertexDescriptor& outDesc)
	{
		uint32 currentOffset = 0;
		
		FUR_ASSERT(numAttributes == outDesc.m_numAttributes);
		
		for(uint32 i=0; i<numAttributes; ++i)
		{
			auto& outAttribute = outDesc.m_attributes[i];
			outAttribute.m_bufferIndex = bufferIndex;
			outAttribute.m_offset = currentOffset;
			
			VertexLayoutAttribute attribute = attributes[i];
			switch(attribute)
			{
				case VertexLayoutAttribute::Position:
				case VertexLayoutAttribute::Color:
				case VertexLayoutAttribute::Normal:
				case VertexLayoutAttribute::Tangent:
				case VertexLayoutAttribute::Binromal:
					outAttribute.m_format = gpu::VertexFormat::Float3;
					currentOffset += sizeof(math::Vector3);
					break;
					
				case VertexLayoutAttribute::UV:
					outAttribute.m_format = gpu::VertexFormat::Float2;
					currentOffset += sizeof(math::Vector2);
					break;
					
				case VertexLayoutAttribute::SkinJointsA:
				case VertexLayoutAttribute::SkinJointsB:
					outAttribute.m_format = gpu::VertexFormat::UInt4;
					currentOffset += sizeof(math::UintVector4);
					break;
					
				case VertexLayoutAttribute::SkinWeightsA:
				case VertexLayoutAttribute::SkinWeightsB:
					outAttribute.m_format = gpu::VertexFormat::Float4;
					currentOffset += sizeof(math::Vector4);
					break;
					
				default:
					FUR_ASSERT(false);
					break;
			}
		}
	}
}

void GeometryBuilder::ReserveMemory(uint32 numVertices)
{
	m_vertices.reserve(numVertices);
}

void GeometryBuilder::AddVertex(const SimpleVertexLayout& layout)
{
	m_vertices.push_back(layout);
}

void GeometryBuilder::AddIndex(uint32 index)
{
	m_indices.push_back(index);
}

void GeometryBuilder::BuildGeometryBuffer(gpu::Device* device, const VertexLayoutAttribute* attributes, uint32 numAttributes, uint32 bufferIndex, UniquePtr<gpu::Buffer>& outBuffer, gpu::VertexDescriptor* outVertexDescription) const
{
	// create temporary buffer
	uint32 vertexSize = helper::GetVertexSize(attributes, numAttributes);
	uint32 bufferSize = vertexSize * (uint32)m_vertices.size();
	float* data = reinterpret_cast<float*>(mem::Allocate<mem::Tag::Temporary>(bufferSize));
	
	uint32 currentPosition = 0;
	
	// fill temporery buffer with vertex data
	for(uint32 i=0; i<m_vertices.size(); ++i)
	{
		for(uint32 j=0; j<numAttributes; ++j)
		{
			helper::CopyAttributeValue(data, bufferSize, currentPosition, m_vertices[i], attributes[j]);
		}
	}
	
	// create vertex buffer
	{
		gpu::BufferDesc desc;
		desc.m_data = data;
		desc.m_size = bufferSize;
		outBuffer = device->CreateBuffer(desc);
	}
	
	// free temporary buffer
	mem::Free<mem::Tag::Temporary>(reinterpret_cast<uint8*>(data));
	
	// fill vertex descriptor
	if(outVertexDescription)
	{
		helper::FillVertexDescriptor(attributes, numAttributes, bufferIndex, *outVertexDescription);
		
		FUR_ASSERT(bufferIndex < outVertexDescription->m_numLayouts);
		outVertexDescription->m_layouts[bufferIndex].m_stride = vertexSize;
	}
}

void GeometryBuilder::BuildGeometryBuffer(gpu::Device* device, const VertexLayoutAttribute* attributes, uint32 numAttributes, uint32 bufferIndex, gpu::Buffer& outBuffer, gpu::VertexDescriptor* outVertexDescription) const
{
	// create temporary buffer
	uint32 vertexSize = helper::GetVertexSize(attributes, numAttributes);
	uint32 bufferSize = vertexSize * (uint32)m_vertices.size();
	float* data = reinterpret_cast<float*>(mem::Allocate<mem::Tag::Temporary>(bufferSize));
	
	uint32 currentPosition = 0;
	
	// fill temporery buffer with vertex data
	for(uint32 i=0; i<m_vertices.size(); ++i)
	{
		for(uint32 j=0; j<numAttributes; ++j)
		{
			helper::CopyAttributeValue(data, bufferSize, currentPosition, m_vertices[i], attributes[j]);
		}
	}
	
	// create vertex buffer
	{
		gpu::BufferDesc desc;
		desc.m_data = data;
		desc.m_size = bufferSize;
		device->CreateBuffer(desc, outBuffer);
	}
	
	// free temporary buffer
	mem::Free<mem::Tag::Temporary>(reinterpret_cast<uint8*>(data));
	
	// fill vertex descriptor
	if(outVertexDescription)
	{
		helper::FillVertexDescriptor(attributes, numAttributes, bufferIndex, *outVertexDescription);
		
		FUR_ASSERT(bufferIndex < outVertexDescription->m_numLayouts);
		outVertexDescription->m_layouts[bufferIndex].m_stride = vertexSize;
	}
}

void GeometryBuilder::BuildIndexBuffer(gpu::Device* device, UniquePtr<gpu::Buffer>& outBuffer) const
{
	const uint32 size = (uint32)m_indices.size() * sizeof(uint32);
	gpu::BufferDesc desc;
	desc.m_data = m_indices.data();
	desc.m_size = size;
	outBuffer = device->CreateBuffer(desc);
}

void GeometryBuilder::BuildIndexBuffer(gpu::Device* device, gpu::Buffer& outBuffer) const
{
	const uint32 size = (uint32)m_indices.size() * sizeof(uint32);
	gpu::BufferDesc desc;
	desc.m_data = m_indices.data();
	desc.m_size = size;
	device->CreateBuffer(desc, outBuffer);
}

void rend::BuildMeshChunk(const GeometryBuilder& builder, gpu::Device* device, const VertexLayoutAttribute* attributes, uint32 numAttributes, uint32 bufferIndex, rend::MeshChunk& outMeshChunk)
{
	builder.BuildGeometryBuffer(device, attributes, numAttributes, bufferIndex, outMeshChunk.m_vertices, nullptr);
	builder.BuildIndexBuffer(device, outMeshChunk.m_indices);
	outMeshChunk.m_numVertices = builder.NumVertices();
	outMeshChunk.m_numIndices = builder.NumIndices();
}

