#include "pch.h"
#include "geometryUtils.h"
#include "geometryBuilder.h"

using namespace rend;
using namespace math;

void rend::shape::AddBoxShape(GeometryBuilder& builder, math::Vector3 minPoint, math::Vector3 maxPoint, math::Vector4 color)
{
	SimpleVertexLayout v;
	
	const math::Vector3& m = minPoint;
	const math::Vector3& p = maxPoint;
	
	v.m_position = m;
	builder.AddVertex(v);
	
	v.m_position = {m.x, m.y, p.x};
	builder.AddVertex(v);
	
	v.m_position = {m.x, m.y, p.z};
	builder.AddVertex(v);
}

void rend::shape::AddWorldAxesShape(GeometryBuilder& builder, math::Vector3 position)
{
	SimpleVertexLayout v;
	
	const math::Vector3& p = position;
	
	const math::Vector3 axes[3] =
	{
		{1, 0, 0},
		{0, 1, 0},
		{0, 0, 1}
	};
	
	// x axis
	v.m_color = {1, 0, 0};
	v.m_position = p;
	builder.AddVertex(v);
	v.m_position = p + axes[0];
	builder.AddVertex(v);
	
	// y axis
	v.m_color = {0, 1, 0};
	v.m_position = p;
	builder.AddVertex(v);
	v.m_position = p + axes[1];
	builder.AddVertex(v);
	
	// z axis
	v.m_color = {0, 0, 1};
	v.m_position = p;
	builder.AddVertex(v);
	v.m_position = p + axes[2];
	builder.AddVertex(v);
}

void rend::shape::AddLine(GeometryBuilder& builder, math::Vector3 a, math::Vector3 b, math::Vector3 color)
{
	SimpleVertexLayout v;
	v.m_color = color;
	
	v.m_position = a;
	builder.AddVertex(v);
	
	v.m_position = b;
	builder.AddVertex(v);
}
