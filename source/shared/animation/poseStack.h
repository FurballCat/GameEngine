#pragma once

namespace anim
{
	struct Pose;
	
	class ANIMATION_API PoseStack
	{
	public:
		PoseStack(uint8* buffer, uint32 bufferSize, uint16 numTransformsPerPose, uint16 numChannelsPerPose);
		
		void PushPose(uint32 count = 1)
		{
			FUR_ASSERT(m_numPoses + count < m_maxNumPoses);
			m_numPoses += count;
		}
		
		void PopPose(uint32 count = 1)
		{
			FUR_ASSERT(m_numPoses - count >= 0);
			m_numPoses -= count;
		}
		
		void GetPose(Pose* pose, uint32 depth) const;
		uint32 GetPoseSizeInBytes() const;
		uint32 GetTransformsSizeInBytes() const;
		uint32 GetChannelsSizeInBytes() const;
		
		static const uint32 POSE_ALIGNMENT = 16;
		
	private:
		uint8* m_buffer = nullptr;
		
		const uint32 m_bufferSize = 0;
		uint32 m_numPoses = 0;
		uint32 m_maxNumPoses = 0;
		
		const uint16 m_numTransformsPerPose = 0;
		const uint16 m_numChannelsPerPose = 0;
	};
}
