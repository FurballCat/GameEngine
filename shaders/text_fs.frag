#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

layout(binding = 1) uniform sampler2D fontSampler;

void main()
{
    outColor = texture(fontSampler, fragTexCoord);
	if(outColor.x == 0.0f)
		discard;
	
	outColor.xyz *= fragColor.xyz;	// skinning weight
	outColor.w = 1.0f;
}
