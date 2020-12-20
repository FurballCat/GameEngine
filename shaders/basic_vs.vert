#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 0) uniform UniformBufferObject {
	mat4 model;
	mat4 view;
	mat4 proj;
} ubo;

layout(binding = 1) uniform SkinningBuffer {
	mat4 bones[512];
} skin;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in ivec4 skinIndices;
layout(location = 4) in vec4 skinWeights;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void main()
{
	const vec4 pos = vec4(inPosition, 1.0f);
	const vec4 norm = vec4(inNormal, 0.0f);
	vec4 posSkinned = {0.0f, 0.0f, 0.0f, 0.0f};
	vec4 normSkinned = {0.0f, 0.0f, 0.0f, 0.0f};

	for(int i=0; i<4; ++i)
	{
		if(skinIndices[i] >= 0)
		{
			const mat4 bone = skin.bones[skinIndices[i]];
			const float weight = skinWeights[i];
			
			posSkinned += (bone * pos) * weight;
			normSkinned += (bone * norm) * weight;
		}
	}
	
	posSkinned.w = 1.0f;
	
	gl_Position = ubo.proj * ubo.view * ubo.model * posSkinned;
	
	const vec3 colors[2] = {{1.0f, 0.0f, 0.0f}, {0.0f, 1.0f, 0.0f}};
	
    fragColor = colors[skinIndices.x] * skinWeights.x + colors[skinIndices.y] * skinWeights.y;
	fragTexCoord = inTexCoord;
}
