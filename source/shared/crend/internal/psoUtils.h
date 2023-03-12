/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#include "ccore/types.h"

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

void fr_pso_init_input_assembly_state_triangle_list(VkPipelineInputAssemblyStateCreateInfo* info);
void fr_pso_init_input_assembly_state_line_list(VkPipelineInputAssemblyStateCreateInfo* info);

void fr_pso_init_viewport(f32 width, f32 height, VkViewport* viewport);

void fr_pso_init_scissor(VkExtent2D swapChainExtent, VkRect2D* scissor);

void fr_pso_init_viewport_state(VkViewport* viewport, VkRect2D* scissor,
								VkPipelineViewportStateCreateInfo* viewportState);

void fr_pso_init_rasterization_state_polygon_fill(VkPipelineRasterizationStateCreateInfo* rasterizer);
void fr_pso_init_rasterization_state_wireframe_no_cull(VkPipelineRasterizationStateCreateInfo* rasterizer);

void fr_pso_init_multisampling_state(VkPipelineMultisampleStateCreateInfo* multisampling);

void fr_pso_init_color_blend_attachment_state(VkPipelineColorBlendAttachmentState* colorBlendAttachment);
void fr_pso_init_color_blend_attachment_state_blending(VkPipelineColorBlendAttachmentState* colorBlendAttachment);

void fr_pso_init_color_blend_state(VkPipelineColorBlendAttachmentState* colorBlendAttachment,
							  VkPipelineColorBlendStateCreateInfo* colorBlending);

void fr_pso_init_layout(VkDescriptorSetLayout* descriptorSetLayout, VkPipelineLayoutCreateInfo* pipelineLayoutInfo);

void fr_pso_init_depth_stencil_state(VkPipelineDepthStencilStateCreateInfo* depthStencil);
void fr_pso_init_depth_stencil_state_no_depth_test(VkPipelineDepthStencilStateCreateInfo* depthStencil);

void fr_attachment_init_color(u32 attachmentIndex,
							  VkFormat colorFormat,
							  VkAttachmentDescription* colorAttachment,
							  VkAttachmentReference* colorAttachmentRef);

void fr_attachment_init_depth(u32 attachmentIndex,
							  VkFormat depthFormat,
							  VkAttachmentDescription* depthAttachment,
							  VkAttachmentReference* depthAttachmentRef);

void fr_subpass_init_color_depth(VkAttachmentReference* colorAttachmentRef, VkAttachmentReference* depthAttachmentRef,
								 VkSubpassDescription* subpass, VkSubpassDependency* dependency);

VkResult fr_render_pass_create_color_depth(VkDevice device, VkFormat colorFormat, VkFormat depthFormat,
										   VkRenderPass* renderPass, fc_alloc_callbacks_t* pAllocCallbacks);

void fr_pso_init_shader_stages_simple(VkShaderModule vertexShader, const char* vsFuncName,
									  VkShaderModule fragmentShader, const char* fsFuncName,
									  VkPipelineShaderStageCreateInfo outInfo[2]);
