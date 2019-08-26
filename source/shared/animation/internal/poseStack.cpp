#include "pch.h"
#include "poseStack.h"

using namespace anim;
using namespace math;

PoseStack::PoseStack(uint8* buffer, uint32 bufferSize, uint16 numTransformsPerPose, uint16 numChannelsPerPose)
	: m_buffer(buffer)
	, m_bufferSize(bufferSize)
	, m_numTransformsPerPose(numTransformsPerPose)
	, m_numChannelsPerPose(numChannelsPerPose)
{
	FUR_ASSERT(GetPoseSizeInBytes() > 0);
	FUR_ASSERT(buffer != nullptr);
	FUR_ASSERT((reinterpret_cast<uintptr>(buffer) % POSE_ALIGNMENT) == 0);
	
	m_maxNumPoses = bufferSize / GetPoseSizeInBytes();
	
	FUR_ASSERT(m_maxNumPoses > 0);
}

void PoseStack::GetPose(Pose* pose, uint32 depth) const
{
	const uint32 poseSize = GetPoseSizeInBytes();
	uint8* ptr = m_buffer + (m_numPoses - depth) * poseSize;
	
	pose->m_transforms = reinterpret_cast<Transform*>(ptr);
	
	uint32 transformsSize = GetTransformsSizeInBytes();
	pose->m_channels = reinterpret_cast<float*>(ptr + transformsSize);
}

uint32 PoseStack::GetPoseSizeInBytes() const
{
	uint32 poseSizeWithoutPadding = GetTransformsSizeInBytes() + GetChannelsSizeInBytes();
	return poseSizeWithoutPadding + (poseSizeWithoutPadding % POSE_ALIGNMENT);
}

uint32 PoseStack::GetTransformsSizeInBytes() const
{
	return m_numTransformsPerPose * sizeof(Transform);
}

uint32 PoseStack::GetChannelsSizeInBytes() const
{
	return m_numChannelsPerPose * sizeof(float);
}
