#pragma once

namespace anim
{
	class ANIMATION_API Animation
	{
	public:
		uint32 NumJointSamples() const { return (uint32)m_joints.size(); }
		uint32 NumChannelSamples() const { return (uint32)m_channels.size(); }
		
	private:
		struct AnimJointSample
		{
			uint16 m_time;
			uint16 m_joint_type2_index14;
			math::Vector4 m_value;	// todo: store it in int16[3], restore one component (see bitsquid blog for more info)
		};
		
		struct AnimChannelSample
		{
			uint16 m_time;
			uint16 m_channel;
			float m_value;
		};
		
		DynArray<AnimJointSample> m_joints;
		DynArray<AnimChannelSample> m_channels;
	};
	
	constexpr uint16 GetJointIndex(uint16 joint_type2_index14) { return joint_type2_index14 & 0x2FFF; }
	constexpr uint16 GetJointComponentType(uint16 joint_type2_index14) { return joint_type2_index14 & 0xC000; }
	
	constexpr bool IsJointComponentTypeTranslation(uint16 joint_type2) { return joint_type2 == 0x4000; }
	constexpr bool IsJointComponentTypeRotation(uint16 joint_type2) { return joint_type2 == 0x8000; }
}
