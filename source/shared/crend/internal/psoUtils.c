/* Copyright (c) 2016-2020 Furball Cat */

#include "vulkan.h"
#include "ccore/furAssert.h"
#include <stdbool.h>
#include <string.h>

#include "psoUtils.h"

void fcRenderPSOInitInputAssemblyStateTriangleList(VkPipelineInputAssemblyStateCreateInfo* info)
{
	info->sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
	info->topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
	info->primitiveRestartEnable = VK_FALSE;
}

void fcRenderPSOInitInputAssemblyStateLineList(VkPipelineInputAssemblyStateCreateInfo* info)
{
	info->sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
	info->topology = VK_PRIMITIVE_TOPOLOGY_LINE_LIST;
	info->primitiveRestartEnable = VK_FALSE;
}

void fcRenderPSOInitViewport(f32 width, f32 height, VkViewport* viewport)
{
	viewport->x = 0.0f;
	viewport->y = 0.0f;
	viewport->width = width;
	viewport->height = height;
	viewport->minDepth = 0.0f;
	viewport->maxDepth = 1.0f;
}

void fcRenderPSOInitScissor(VkExtent2D swapChainExtent, VkRect2D* scissor)
{
	scissor->offset.x = 0;
	scissor->offset.y = 0;
	scissor->extent = swapChainExtent;
}

void fcRenderPSOInitViewportState(VkViewport* viewport, VkRect2D* scissor,
								VkPipelineViewportStateCreateInfo* viewportState)
{
	viewportState->sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
	viewportState->viewportCount = 1;
	viewportState->pViewports = viewport;
	viewportState->scissorCount = 1;
	viewportState->pScissors = scissor;
}

void fcRenderPSOInitRasterizationStatePolygonFill(VkPipelineRasterizationStateCreateInfo* rasterizer)
{
	rasterizer->sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
	rasterizer->depthClampEnable = VK_FALSE;
	rasterizer->rasterizerDiscardEnable = VK_FALSE;
	rasterizer->polygonMode = VK_POLYGON_MODE_FILL;
	rasterizer->lineWidth = 1.0f;
	rasterizer->cullMode = VK_CULL_MODE_BACK_BIT;
	rasterizer->frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
	rasterizer->depthBiasEnable = VK_FALSE;
	rasterizer->depthBiasConstantFactor = 0.0f; // Optional
	rasterizer->depthBiasClamp = 0.0f; // Optional
	rasterizer->depthBiasSlopeFactor = 0.0f; // Optional
}

void fcRenderPSOInitRasterizationStateWireframeNoCull(VkPipelineRasterizationStateCreateInfo* rasterizer)
{
	rasterizer->sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
	rasterizer->depthClampEnable = VK_FALSE;
	rasterizer->rasterizerDiscardEnable = VK_FALSE;
	rasterizer->polygonMode = VK_POLYGON_MODE_LINE;
	rasterizer->lineWidth = 1.0f;
	rasterizer->cullMode = VK_CULL_MODE_NONE;
	rasterizer->frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
	rasterizer->depthBiasEnable = VK_FALSE;
	rasterizer->depthBiasConstantFactor = 0.0f; // Optional
	rasterizer->depthBiasClamp = 0.0f; // Optional
	rasterizer->depthBiasSlopeFactor = 0.0f; // Optional
}

void fcRenderPSOInitMultisamplingState(VkPipelineMultisampleStateCreateInfo* multisampling)
{
	multisampling->sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
	multisampling->sampleShadingEnable = VK_FALSE;
	multisampling->rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
	multisampling->minSampleShading = 1.0f; // Optional
	multisampling->pSampleMask = NULL; // Optional
	multisampling->alphaToCoverageEnable = VK_FALSE; // Optional
	multisampling->alphaToOneEnable = VK_FALSE; // Optional
}

void fcRenderPSOInitColorBlendAttachmentState(VkPipelineColorBlendAttachmentState* colorBlendAttachment)
{
	colorBlendAttachment->colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
	colorBlendAttachment->blendEnable = VK_FALSE;
	colorBlendAttachment->srcColorBlendFactor = VK_BLEND_FACTOR_ONE; // Optional
	colorBlendAttachment->dstColorBlendFactor = VK_BLEND_FACTOR_ZERO; // Optional
	colorBlendAttachment->colorBlendOp = VK_BLEND_OP_ADD; // Optional
	colorBlendAttachment->srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE; // Optional
	colorBlendAttachment->dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO; // Optional
	colorBlendAttachment->alphaBlendOp = VK_BLEND_OP_ADD; // Optional
}

void fcRenderPSOInitColorBlendAttachmentStateBlending(VkPipelineColorBlendAttachmentState* colorBlendAttachment)
{
	colorBlendAttachment->colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
	colorBlendAttachment->blendEnable = VK_TRUE;
	colorBlendAttachment->srcColorBlendFactor = VK_BLEND_FACTOR_SRC_ALPHA;
	colorBlendAttachment->dstColorBlendFactor = VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
	colorBlendAttachment->colorBlendOp = VK_BLEND_OP_ADD;
	colorBlendAttachment->srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
	colorBlendAttachment->dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO;
	colorBlendAttachment->alphaBlendOp = VK_BLEND_OP_ADD;
}

void fcRenderPSOInitColorBlendState(VkPipelineColorBlendAttachmentState* colorBlendAttachment,
							  VkPipelineColorBlendStateCreateInfo* colorBlending)
{
	colorBlending->sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
	colorBlending->logicOpEnable = VK_FALSE;
	colorBlending->logicOp = VK_LOGIC_OP_COPY; // Optional
	colorBlending->attachmentCount = 1;
	colorBlending->pAttachments = colorBlendAttachment;
	colorBlending->blendConstants[0] = 0.0f; // Optional
	colorBlending->blendConstants[1] = 0.0f; // Optional
	colorBlending->blendConstants[2] = 0.0f; // Optional
	colorBlending->blendConstants[3] = 0.0f; // Optional
}

void fcRenderPSOInitLayout(VkDescriptorSetLayout* descriptorSetLayout, VkPipelineLayoutCreateInfo* pipelineLayoutInfo)
{
	pipelineLayoutInfo->sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
	pipelineLayoutInfo->setLayoutCount = 1; // Optional, 1 because of uniform buffer
	pipelineLayoutInfo->pSetLayouts = descriptorSetLayout; // Optional, setting it befause of uniform buffer
	pipelineLayoutInfo->pushConstantRangeCount = 0; // Optional
	pipelineLayoutInfo->pPushConstantRanges = NULL; // Optional
}

void fcRenderPSOInitDepthStencilState(VkPipelineDepthStencilStateCreateInfo* depthStencil)
{
	depthStencil->sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
	depthStencil->depthTestEnable = VK_TRUE;
	depthStencil->depthWriteEnable = VK_TRUE;
	depthStencil->depthCompareOp = VK_COMPARE_OP_LESS;
	depthStencil->depthBoundsTestEnable = VK_FALSE;
	depthStencil->minDepthBounds = 0.0f; // Optional
	depthStencil->maxDepthBounds = 1.0f; // Optional
	depthStencil->stencilTestEnable = VK_FALSE;
	// depthStencil->front = {}; // Optional - cleared at the beginning by depthStencil = {}
	// depthStencil->back = {}; // Optional
}

void fcRenderPSOInitDepthStencilStateNoDepthTest(VkPipelineDepthStencilStateCreateInfo* depthStencil)
{
	depthStencil->sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
	depthStencil->depthTestEnable = VK_FALSE;
	depthStencil->depthWriteEnable = VK_FALSE;
	depthStencil->depthCompareOp = VK_COMPARE_OP_ALWAYS;
	depthStencil->depthBoundsTestEnable = VK_FALSE;
	depthStencil->minDepthBounds = 0.0f; // Optional
	depthStencil->maxDepthBounds = 1.0f; // Optional
	depthStencil->stencilTestEnable = VK_FALSE;
	// depthStencil->front = {}; // Optional - cleared at the beginning by depthStencil = {}
	// depthStencil->back = {}; // Optional
}

void fcRenderAttachmentInitColor(u32 attachmentIndex,
							  VkFormat colorFormat,
							  VkAttachmentDescription* colorAttachment,
							  VkAttachmentReference* colorAttachmentRef)
{
	colorAttachment->flags = 0;
	colorAttachment->format = colorFormat;
	colorAttachment->samples = VK_SAMPLE_COUNT_1_BIT;
	colorAttachment->loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
	colorAttachment->storeOp = VK_ATTACHMENT_STORE_OP_STORE;
	colorAttachment->stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
	colorAttachment->stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
	colorAttachment->initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
	colorAttachment->finalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
	
	colorAttachmentRef->attachment = 0;
	colorAttachmentRef->layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
}

void fcRenderAttachmentInitDepth(u32 attachmentIndex,
							  VkFormat depthFormat,
							  VkAttachmentDescription* depthAttachment,
							  VkAttachmentReference* depthAttachmentRef)
{
	depthAttachment->flags = 0;
	depthAttachment->format = depthFormat;
	depthAttachment->samples = VK_SAMPLE_COUNT_1_BIT;
	depthAttachment->loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
	depthAttachment->storeOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
	depthAttachment->stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
	depthAttachment->stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
	depthAttachment->initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
	depthAttachment->finalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
	
	depthAttachmentRef->attachment = attachmentIndex;
	depthAttachmentRef->layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
}

void fcRenderSubpassInitColorDepth(VkAttachmentReference* colorAttachmentRef, VkAttachmentReference* depthAttachmentRef,
								 VkSubpassDescription* subpass, VkSubpassDependency* dependency)
{
	subpass->pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
	subpass->colorAttachmentCount = 1;
	subpass->pColorAttachments = colorAttachmentRef;
	subpass->pDepthStencilAttachment = depthAttachmentRef;
	
	dependency->srcSubpass = VK_SUBPASS_EXTERNAL;
	dependency->dstSubpass = 0;
	dependency->srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
	dependency->srcAccessMask = 0;
	dependency->dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
	dependency->dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT | VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
}

VkResult fcRenderPassCreateColorDepth(VkDevice device, VkFormat colorFormat, VkFormat depthFormat,
										   VkRenderPass* renderPass, FcAllocator* pAllocCallbacks)
{
	// create render pass
	VkAttachmentDescription colorAttachment = {0};
	VkAttachmentReference colorAttachmentRef = {0};
	fcRenderAttachmentInitColor(0, colorFormat, &colorAttachment, &colorAttachmentRef);
	
	VkAttachmentDescription depthAttachment = {0};
	VkAttachmentReference depthAttachmentRef = {0};
	fcRenderAttachmentInitDepth(1, depthFormat, &depthAttachment, &depthAttachmentRef);
	
	// subpass and dependency
	VkSubpassDescription subpass = {0};
	VkSubpassDependency dependency = {0};
	fcRenderSubpassInitColorDepth(&colorAttachmentRef, &depthAttachmentRef, &subpass, &dependency);
	
	VkAttachmentDescription attachments[2] = {colorAttachment, depthAttachment};
	const u32 numAttachments = 2;
	
	// create render pass
	VkRenderPassCreateInfo renderPassInfo = {0};
	renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
	renderPassInfo.attachmentCount = numAttachments;
	renderPassInfo.pAttachments = attachments;
	renderPassInfo.subpassCount = 1;
	renderPassInfo.pSubpasses = &subpass;
	
	renderPassInfo.dependencyCount = 1;
	renderPassInfo.pDependencies = &dependency;
	
	return vkCreateRenderPass(device, &renderPassInfo, NULL, renderPass);
}

void fcRenderPSOInitShaderStagesSimple(VkShaderModule vertexShader, const char* vsFuncName,
									  VkShaderModule fragmentShader, const char* fsFuncName,
									  VkPipelineShaderStageCreateInfo outInfo[2])
{
	VkPipelineShaderStageCreateInfo vertShaderStageInfo = {0};
	vertShaderStageInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
	vertShaderStageInfo.stage = VK_SHADER_STAGE_VERTEX_BIT;
	vertShaderStageInfo.module = vertexShader;
	vertShaderStageInfo.pName = vsFuncName;
	
	VkPipelineShaderStageCreateInfo fragShaderStageInfo = {0};
	fragShaderStageInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
	fragShaderStageInfo.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
	fragShaderStageInfo.module = fragmentShader;
	fragShaderStageInfo.pName = fsFuncName;
	
	outInfo[0] = vertShaderStageInfo;
	outInfo[1] = fragShaderStageInfo;
}
