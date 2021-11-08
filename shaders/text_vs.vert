#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 0) uniform UniformBufferObject {
	mat4 model;
	mat4 view;
	mat4 proj;
} ubo;

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec2 inTexCoord;
layout(location = 2) in vec3 inColor;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void main()
{
	const vec4 pos = vec4(inPosition.x * 0.5f, inPosition.y * 0.5f, 0.0f, 1.0f);
	
	gl_Position = ubo.proj * ubo.view * ubo.model * pos;
	
    fragColor = inColor;
	fragTexCoord = inTexCoord;
}
