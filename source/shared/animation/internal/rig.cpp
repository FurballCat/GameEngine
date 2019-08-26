#include "pch.h"
#include "rig.h"

using namespace anim;

void Rig::GetReferencePose(Pose& pose) const
{
	FUR_ASSERT(pose.m_numTransforms == NumJoints());
	FUR_ASSERT(pose.m_numChannels == NumChannels());
	
	mem::MemoryCopy(pose.m_channels, m_referenceChannels.data(), NumChannels() * sizeof(float));
	mem::MemoryCopy(pose.m_transforms, m_referenceTransforms.data(), NumJoints() * sizeof(math::Transform));
}
