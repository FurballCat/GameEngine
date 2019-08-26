#pragma once

namespace anim
{
	// you only need rig to create animation player
	class AnimPlayer
	{
	public:
		// done casually
		void SetStartTime(uint64 time) { m_startGlobalTime = time; }
		
		// batched for all animations
		void Fetch(const Animation* animation, uint64 currentGlobalTime);
		
		// per anim command
		void Sample(Pose& pose) const;
		
		// done once at the beginning, fills active arrays with reference pose or identity pose
		// regular - fills default active arrays with reference pose
		static void CreateRegular(const Rig* rig, AnimPlayer& player);
		
		// additive - fills default active arrays with identity pose
		static void CreateAdditive(const Rig* rig, AnimPlayer& player);
		
	private:
		struct JointActiveArray
		{
			uint16 m_joint_type2_index14;
			uint16 m_times[4];
			math::Vector4 m_values[4];
		};
		
		struct ChannelActiveArray
		{
			uint16 m_index;
			uint16 m_times[4];
			float m_values[4];
		};
		
		uint64 m_startGlobalTime;
		uint64 m_lastFetchedCurrentGlobalTime;
		uint16 m_duration;
		
		uint16 m_nextJointSampleIndex;
		JointActiveArray* m_jointActiveArrays;
		ChannelActiveArray* m_channelActiveArrays;
		uint16 m_numJointActiveArrays;
		uint16 m_numChannelActiveArrays;
	};
	
	// cases of animation update:
	// 1) Object is visible, we need to update logic (collect sound events), fetch animations and sample pose
	// 2) Object is not visible, but nearby, we need to update logic (collect sound events), but no need for animation fetching or pose sampling
	// 3) Object is not visible and far away, we don't need to update anything, cost is zero
}
