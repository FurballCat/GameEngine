#pragma once

namespace anim
{
	struct Pose;
	
	class ANIMATION_API Rig
	{
	public:
		uint16 NumJoints() const { return (uint16)m_referenceTransforms.size(); }
		uint16 NumChannels() const { return (uint16)m_referenceChannels.size(); }
	
		void GetReferencePose(Pose& pose) const;
		
	private:
		DynArray<int16> m_parentIndices;
		DynArray<math::Transform> m_referenceTransforms;
		DynArray<float> m_referenceChannels;
	};
}
