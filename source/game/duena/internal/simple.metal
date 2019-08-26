#include <metal_stdlib>

using namespace metal;

constexpr sampler textureSampler(mag_filter::linear, min_filter::linear);
constexpr sampler textTextureSampler(mag_filter::nearest, min_filter::nearest);

struct OneObjectInfo
{
    float4 m_position;
    float4 m_color;
};

struct ConstantBufferData
{
    OneObjectInfo m_objects[40];
};

struct VertexShaderOutput
{
    float4 m_position [[position]];
    float3 m_color;
};

vertex VertexShaderOutput vertFunc( const device packed_float3* vertexArray [[buffer(0)]], constant ConstantBufferData& data [[buffer(1)]], unsigned int vID[[vertex_id]])
{
    VertexShaderOutput output;
    
    output.m_position = float4(vertexArray[vID % 3], 1.0);
    output.m_color = float3(0.0f, 0.8f, 0.0f);
    
    OneObjectInfo info = data.m_objects[vID / 3];
    
    output.m_position.xyz *= info.m_position.w;
    output.m_position.xyz += info.m_position.xyz;
    output.m_color = info.m_color.xyz;
    
    return output;
}

fragment half4 fragFunc(VertexShaderOutput input [[stage_in]])
{
    return half4(input.m_color.x, input.m_color.y, input.m_color.z, 1.0f);
}

struct CameraData
{
	float4x4 m_worldToCamera;
	float4x4 m_projection;
};

vertex VertexShaderOutput simpleVertexShader( const device packed_float3* vertexArray [[buffer(0)]], constant CameraData& camera [[buffer(1)]], unsigned int vID[[vertex_id]])
{
	VertexShaderOutput output;
	
	output.m_position = float4(vertexArray[vID * 2], 1.0);
	output.m_position = camera.m_worldToCamera * output.m_position;
	output.m_position = camera.m_projection * output.m_position;
	output.m_color = vertexArray[vID * 2 + 1];
	
	return output;
}

struct Vertex
{
	packed_float3 m_position;
	packed_float3 m_normal;
	packed_float2 m_uv;
};

struct FragmentInput
{
	float4 m_position [[position]];
	float3 m_normal;
	float3 m_color;
	float2 m_uv;
};

struct ObjectMatrices
{
	float4x4 m_modelToWorld;
};

vertex FragmentInput regularVertexShader(const device Vertex* vertices [[buffer(0)]], const device uint* indices [[buffer(1)]],
											  constant CameraData& camera [[buffer(2)]], constant ObjectMatrices& matrices [[buffer(3)]],
										 unsigned int vID [[vertex_id]])
{
	FragmentInput output;
	
	const uint index = indices[vID];
	
	output.m_position = float4(vertices[index].m_position, 1.0f);
	output.m_position = matrices.m_modelToWorld * output.m_position;
	output.m_position = camera.m_worldToCamera * output.m_position;
	output.m_position = camera.m_projection * output.m_position;
	
	float3x3 worldToCamera3x3;
	worldToCamera3x3[0].xyz = camera.m_worldToCamera[0].xyz;
	worldToCamera3x3[1].xyz = camera.m_worldToCamera[1].xyz;
	worldToCamera3x3[2].xyz = camera.m_worldToCamera[2].xyz;
	
	output.m_normal = worldToCamera3x3 * float3(vertices[index].m_normal);
	output.m_uv = vertices[index].m_uv;
	output.m_color = float3(1, 1, 1);
	
	return output;
}

vertex FragmentInput instancedVertexShader(const device Vertex* vertices [[buffer(0)]], const device uint* indices [[buffer(1)]],
											  constant CameraData& camera [[buffer(2)]],
										   const device float4x4* instanceMatrices [[buffer(3)]], const device packed_float3* instanceColors [[buffer(4)]],
										   uint vID [[vertex_id]], uint instanceID [[instance_id]])
{
	FragmentInput output;
	
	const uint index = indices[vID];
	
	output.m_position = float4(vertices[index].m_position, 1.0f);
	output.m_position = instanceMatrices[instanceID] * output.m_position;
	output.m_position = camera.m_worldToCamera * output.m_position;
	output.m_position = camera.m_projection * output.m_position;
	
	float3x3 worldToCamera3x3;
	worldToCamera3x3[0].xyz = camera.m_worldToCamera[0].xyz;
	worldToCamera3x3[1].xyz = camera.m_worldToCamera[1].xyz;
	worldToCamera3x3[2].xyz = camera.m_worldToCamera[2].xyz;
	
	output.m_normal = worldToCamera3x3 * float3(vertices[index].m_normal);
	output.m_uv = vertices[index].m_uv;
	output.m_color = instanceColors[instanceID];
	
	return output;
}

fragment half4 texturedFragmentShader(FragmentInput input [[stage_in]], texture2d<half> albedoTexture [[texture(0)]]
									  , texture2d<half> ambientOcclusionTexture [[texture(1)]])
{
	float2 uv = input.m_uv;
	half4 color = albedoTexture.sample(textureSampler, uv);
	half4 ambientOcclusion = ambientOcclusionTexture.sample(textureSampler, uv);
	return color * ambientOcclusion;
}

fragment half4 checkerFragmentShader(FragmentInput input [[stage_in]])
{
	float3 color = (input.m_normal + float3(1,1,1)) * 0.5f;
	
	// checker pattern
	float2 uv = input.m_uv;
	if( (uv.x - 0.5f) * (uv.y - 0.5f) > 0.0f )
		color *= float3(1.0f, 1.0f, 1.0f);
	else
		color *= float3(0.5f, 0.5f, 0.5f);
	
	return half4(color.x, color.y, color.z, 1.0f);
}

// grass tile test
fragment half4 grassFragmentShader(FragmentInput input [[stage_in]])
{
	float3 color = input.m_color;
	float3 L = normalize(float3(1, 1, 1));
	float i = clamp(dot(input.m_normal, L), 0.45f, 1.0f);
	color *= i;
	return half4(color.x, color.y, color.z, 1.0f);
}

// text shaders
struct TextFragmentInput
{
	float4 m_position [[position]];
	float2 m_uv;
};

struct TextVertex
{
	packed_float2 m_position;
	packed_float2 m_uv;
};

vertex TextFragmentInput textVertexShader(const device TextVertex* vertices [[buffer(0)]], constant CameraData& camera [[buffer(1)]],
										   constant ObjectMatrices& matrices [[buffer(2)]], uint vID [[vertex_id]])
{
	TextFragmentInput output;
	
	output.m_position = float4(vertices[vID].m_position, 0.0f, 1.0f);
	output.m_position = matrices.m_modelToWorld * output.m_position;
	output.m_position = camera.m_worldToCamera * output.m_position;
	output.m_position = camera.m_projection * output.m_position;
	output.m_uv = float2(vertices[vID].m_uv);
	
	return output;
}

fragment half4 textFragmentShader(TextFragmentInput input [[stage_in]], texture2d<half> textAtlasTexture [[texture(0)]])
{
	half4 color = textAtlasTexture.sample(textTextureSampler, input.m_uv);
	if(color.a == 0.0f)
		discard_fragment();
	return color;
	//return half4(input.m_uv.x, input.m_uv.y, 0.0f, 1.0f);
}

// sprite shaders
struct SpriteFrame
{
	packed_float4 m_uv;	// top-left UV [x,y] + width-height [z,w]
};

struct SpriteInstance
{
	float4x4 m_modelToWorld;
	packed_float3 m_color;
	uint m_frame;
	packed_float2 m_positionOffset;
};

struct SpriteFragmentInput
{
	float4 m_position [[position]];
	float2 m_uv;
	float4 m_color;
};

// sprite quad is of size 1x1 [x,y], 0.0..1.0 on x axis and 0.0..1.0 on y axis, with center at [0.0, 0.0], this means it's like a neutral texture coordinats
constant float2 g_spriteVertices[6] = {
	{0.0f, 0.0f},	// left-top
	{1.0f, 1.0f},	// right-bottom
	{1.0f, 0.0f},	// right-top
	{0.0f, 0.0f},	// left-top
	{0.0f, 1.0f},	// left-bottom
	{1.0f, 1.0f},	// right-bottom
};

vertex SpriteFragmentInput spriteVertexShader(const device SpriteFrame* frames [[buffer(0)]], constant CameraData& camera [[buffer(1)]],
										  const device SpriteInstance* instances [[buffer(2)]], uint vID [[vertex_id]], uint iID [[instance_id]])
{
	SpriteFragmentInput output;
	
	float2 spriteVertex = g_spriteVertices[vID];
	
	output.m_position = float4(spriteVertex + instances[iID].m_positionOffset, 0.0f, 1.0f);
	output.m_position = instances[iID].m_modelToWorld * output.m_position;
	output.m_position = camera.m_worldToCamera * output.m_position;
	output.m_position = camera.m_projection * output.m_position;
	
	float4 frameUV = float4(frames[instances[iID].m_frame].m_uv);
	
	output.m_uv = frameUV.xy + spriteVertex.xy * frameUV.zw;
	
	output.m_color = float4(instances[iID].m_color, 1.0f);
	
	return output;
}

fragment half4 spriteFragmentShader(SpriteFragmentInput input [[stage_in]], texture2d<half> spriteAtlasTexture [[texture(0)]])
{
	half4 color = spriteAtlasTexture.sample(textTextureSampler, input.m_uv);
	if(color.a == 0.0f)
		discard_fragment();
		//return half4(1.0f, 0.0f, 1.0f, 1.0f);
	
	return half4(float4(color) * input.m_color);
}

// Physically Based Rendering
constant float PI = 3.14159265359f;
constant float Lambda_GGXV = 0.1f;
constant float Lambda_GGXL = 0.2f;

float Lerp(float a, float b, float alpha)
{
	return a * (1.0f - alpha) + b * alpha;
}

float3 F_Schlick(float3 f0, float f90, float u)
{
	return f0 + (f90 - f0) * pow(1.0f - u, 5.0f);
}

float V_SmithGGXCorrelated(float NdotL, float NdotV, float alphaG)
{
	// Original formulation of G_SmithGGX Correlated
	// lambda_v = (-1 + sqrt(alphaG2 * (1 - NdotL2) / NdotL2 + 1)) * 0.5f;
	// lambda_l = (-1 + sqrt(alphaG2 * (1 - NdotV2) / NdotV2 + 1)) * 0.5f; // G_SmithGGXCorrelated = 1 / (1 + lambda_v + lambda_l);
	// V_SmithGGXCorrelated = G_SmithGGXCorrelated / (4.0f * NdotL * NdotV);
	
	// This is the optimize version
	float alphaG2 = alphaG * alphaG;
	
	// Caution: the "NdotL *" and "NdotV *" are explicitely inversed, this is not a mistake.
	// float Lambda_GGXV = NdotL * sqrt((-NdotV * alphaG2 + NdotV) * NdotV + alphaG2);
	float Lambda_GGXL = NdotV * sqrt((-NdotL * alphaG2 + NdotL) * NdotL + alphaG2);
	return 0.5f / (Lambda_GGXV + Lambda_GGXL);
}

float D_GGX(float NdotH , float m) {
	// Divide by PI is apply later
	float m2 = m * m;
	float f = (NdotH * m2 - NdotH) * NdotH + 1;
	return m2 / (f * f);
}

float Fr_DisneyDiffuse(float NdotV, float NdotL, float LdotH, float linearRoughness)
{
	float energyBias = Lerp(0, 0.5, linearRoughness);
	float energyFactor = Lerp(1.0, 1.0 / 1.51, linearRoughness);
	float fd90 = energyBias + 2.0 * LdotH * LdotH * linearRoughness;
	float3 f0 = float3(1.0f, 1.0f, 1.0f);
	
	float lightScatter = F_Schlick(f0, fd90, NdotL).r;
	float viewScatter = F_Schlick(f0, fd90, NdotV).r;
	
	return lightScatter * viewScatter * energyFactor;
}

half4 PBRFragmentShader(FragmentInput input [[stage_in]]
						, texture2d<half> albedoTexture [[texture(0)]]
						, texture2d<half> normalTexture [[texture(1)]]
						, texture2d<half> roughnessTexture [[texture(2)]]
						, texture2d<half> metalnessTexture [[texture(3)]]
						, texture2d<half> ambientOcclusionTexture [[texture(4)]]
						)
{
	float3 N = input.m_normal;	// todo: add normal mapping
	float3 V = float3(1, 1, 1);
	float3 L = float3(1, 1, 1);
	float roughness = roughnessTexture.sample(textureSampler, input.m_uv).x;
	float linearRoughness = roughness;
	float3 f0 = float3(1, 1, 1);
	float f90 = 0.0f;
	
	// This code is an example of call of previous functions
	float NdotV = abs(dot(N, V)) + 1e-5f;	// avoid artifact
	float3 H = normalize(V + L);
	float LdotH = saturate(dot(L, H));
	float NdotH = saturate(dot(N, H));
	float NdotL = saturate(dot(N, L));
	
	// Specular BRDF
	float3 F = F_Schlick(f0, f90, LdotH);
	float Vis = V_SmithGGXCorrelated(NdotV, NdotL, roughness);
	float D = D_GGX(NdotH, roughness);
	float3 Fr = D * F * Vis / PI;
	
	// Diffuse BRDF
	float Fd = Fr_DisneyDiffuse(NdotV, NdotL, LdotH, linearRoughness) / PI;
	
	half4 albedo = albedoTexture.sample(textureSampler, input.m_uv);
	return albedo * Fd;
}














