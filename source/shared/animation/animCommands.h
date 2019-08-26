#pragma once

namespace anim
{
	struct AnimCmdContext
	{
		const Rig* m_rig;
	};
	
	struct ANIMATION_API AnimCmd_RefPose
	{
		static void Func(void* ctx, const void* cmd);
	};
	
	struct ANIMATION_API AnimCmd_Sample
	{
		static void Func(void* ctx, const void* cmd);
		
		uint16 m_animationID;
	};
	
	struct ANIMATION_API AnimCmd_Blend2
	{
		static void Func(void* ctx, const void* cmd);
		
		float m_alpha;
	};
	
	struct ANIMATION_API AnimCmd_BlendOverride
	{
		static void Func(void* ctx, const void* cmd);
		
		float m_alpha;
		uint16 m_maskID;
	};
	
	struct ANIMATION_API AnimCmd_BlendAdditive
	{
		static void Func(void* ctx, const void* cmd);
		
		float m_weight;
	};
}
