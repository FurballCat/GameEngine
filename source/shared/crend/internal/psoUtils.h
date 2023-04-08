/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#include "ccore/types.h"

typedef struct FcAllocator FcAllocator;

void fcRenderPSOInitInputAssemblyStateTriangleList(VkPipelineInputAssemblyStateCreateInfo* info);
void fcRenderPSOInitInputAssemblyStateLineList(VkPipelineInputAssemblyStateCreateInfo* info);

void fcRenderPSOInitViewport(f32 width, f32 height, VkViewport* viewport);

void fcRenderPSOInitScissor(VkExtent2D swapChainExtent, VkRect2D* scissor);

void fcRenderPSOInitViewportState(VkViewport* viewport, VkRect2D* scissor,
								VkPipelineViewportStateCreateInfo* viewportState);

void fcRenderPSOInitRasterizationStatePolygonFill(VkPipelineRasterizationStateCreateInfo* rasterizer);
void fcRenderPSOInitRasterizationStateWireframeNoCull(VkPipelineRasterizationStateCreateInfo* rasterizer);

void fcRenderPSOInitMultisamplingState(VkPipelineMultisampleStateCreateInfo* multisampling);

void fcRenderPSOInitColorBlendAttachmentState(VkPipelineColorBlendAttachmentState* colorBlendAttachment);
void fcRenderPSOInitColorBlendAttachmentStateBlending(VkPipelineColorBlendAttachmentState* colorBlendAttachment);

void fcRenderPSOInitColorBlendState(VkPipelineColorBlendAttachmentState* colorBlendAttachment,
							  VkPipelineColorBlendStateCreateInfo* colorBlending);

void fcRenderPSOInitLayout(VkDescriptorSetLayout* descriptorSetLayout, VkPipelineLayoutCreateInfo* pipelineLayoutInfo);

void fcRenderPSOInitDepthStencilState(VkPipelineDepthStencilStateCreateInfo* depthStencil);
void fcRenderPSOInitDepthStencilStateNoDepthTest(VkPipelineDepthStencilStateCreateInfo* depthStencil);

void fcRenderAttachmentInitColor(u32 attachmentIndex,
							  VkFormat colorFormat,
							  VkAttachmentDescription* colorAttachment,
							  VkAttachmentReference* colorAttachmentRef);

void fcRenderAttachmentInitDepth(u32 attachmentIndex,
							  VkFormat depthFormat,
							  VkAttachmentDescription* depthAttachment,
							  VkAttachmentReference* depthAttachmentRef);

void fcRenderSubpassInitColorDepth(VkAttachmentReference* colorAttachmentRef, VkAttachmentReference* depthAttachmentRef,
								 VkSubpassDescription* subpass, VkSubpassDependency* dependency);

VkResult fcRenderPassCreateColorDepth(VkDevice device, VkFormat colorFormat, VkFormat depthFormat,
										   VkRenderPass* renderPass, const FcAllocator* allocator);

void fcRenderPSOInitShaderStagesSimple(VkShaderModule vertexShader, const char* vsFuncName,
									  VkShaderModule fragmentShader, const char* fsFuncName,
									  VkPipelineShaderStageCreateInfo outInfo[2]);
