#pragma once

namespace rend
{
	class GeometryBuilder;
	
	namespace shape
	{
		RENDERER_API void AddBoxShape(GeometryBuilder& builder, math::Vector3 minPoint, math::Vector3 maxPoint, math::Vector4 color);
		RENDERER_API void AddWorldAxesShape(GeometryBuilder& builder, math::Vector3 position);
		RENDERER_API void AddLine(GeometryBuilder& builder, math::Vector3 a, math::Vector3 b, math::Vector3 color);
	}
}
