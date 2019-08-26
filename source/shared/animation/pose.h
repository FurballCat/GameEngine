#pragma once

namespace math
{
	struct Transform;
}

namespace anim
{
	struct Pose
	{
		math::Transform* m_transforms;
		float* m_channels;
		
		uint32 m_numTransforms;
		uint32 m_numChannels;
	};
}
