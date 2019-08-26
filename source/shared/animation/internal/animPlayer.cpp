#include "pch.h"
#include "animPlayer.h"

using namespace anim;
using namespace math;

void AnimPlayer::Sample(anim::Pose& pose) const
{
	// sample joints
	for(uint32 i=0; i<m_numJointActiveArrays; ++i)
	{
		const JointActiveArray& data = m_jointActiveArrays[i];
		
		// get index of the transform (joint)
		const uint16 index = GetJointIndex(data.m_joint_type2_index14);
		
		// get component type of the transform (translation or rotation)
		const uint16 type = GetJointComponentType(data.m_joint_type2_index14);
		
		if(index < pose.m_numTransforms)
		{
			// Catmull-Rom spline does interpolation only between point 1 and 2 out of {0, 1, 2, 3}
			const uint16 t1 = data.m_times[1];
			const uint16 t2 = data.m_times[2];
			
			// get delta time between middle samples
			const uint16 delta = t2 - t1;
			
			// get local time of this animation
			const uint16 localTime = Min(0ull, m_startGlobalTime - m_lastFetchedCurrentGlobalTime) % m_duration;
			
			// get time delta from t1 to current local animation time
			const uint16 timeFromT1 = localTime - t1;
			
			// get alpha for Catmull-Rom spline interpolation
			const float t = (float)timeFromT1 / (float)delta;
			
			// calculate interpolated value
			const Vector4 value = math::SplineCatmullRom(data.m_values[0], data.m_values[1], data.m_values[2], data.m_values[3], t);
			
			// assign to proper transform component
			if(IsJointComponentTypeTranslation(type))
			{
				pose.m_transforms[index].translation = value;
			}
			else if(IsJointComponentTypeRotation(type))
			{
				pose.m_transforms[index].rotation = ToQuaternion(value);
			}
		}
	}
	
	// sample channels
	for(uint32 i=0; i<m_numChannelActiveArrays; ++i)
	{
		const ChannelActiveArray& data = m_channelActiveArrays[i];
		
		// get index of the transform (joint)
		const uint16 index = data.m_index;
		
		if(index < pose.m_numTransforms)
		{
			// Catmull-Rom spline does interpolation only between point 1 and 2 out of {0, 1, 2, 3}
			const uint16 t1 = data.m_times[1];
			const uint16 t2 = data.m_times[2];
			
			// get delta time between middle samples
			const uint16 delta = t2 - t1;
			
			// get local time of this animation
			const uint16 localTime = Min(0ull, m_startGlobalTime - m_lastFetchedCurrentGlobalTime) % m_duration;
			
			// get time delta from t1 to current local animation time
			const uint16 timeFromT1 = localTime - t1;
			
			// get alpha for Catmull-Rom spline interpolation
			const float t = (float)timeFromT1 / (float)delta;
			
			// calculate interpolated value
			const float value = math::SplineCatmullRom(data.m_values[0], data.m_values[1], data.m_values[2], data.m_values[3], t);
			
			// assing to proper channel
			pose.m_channels[index] = value;
		}
	}
}
