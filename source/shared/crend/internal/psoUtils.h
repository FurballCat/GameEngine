/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

void fr_pso_init_input_assembly_state_triangle_list(VkPipelineInputAssemblyStateCreateInfo* info);

void fr_pso_init_viewport(float width, float height, VkViewport* viewport);

void fr_pso_init_scissor(VkExtent2D swapChainExtent, VkRect2D* scissor);

void fr_pso_init_viewport_state(VkViewport* viewport, VkRect2D* scissor,
								VkPipelineViewportStateCreateInfo* viewportState);

void fr_pso_init_rasterization_state_polygon_fill(VkPipelineRasterizationStateCreateInfo* rasterizer);
void fr_pso_init_rasterization_state_polygon_line(VkPipelineRasterizationStateCreateInfo* rasterizer);

void fr_pso_init_multisampling_state(VkPipelineMultisampleStateCreateInfo* multisampling);

void fr_pso_init_color_blend_attachment_state(VkPipelineColorBlendAttachmentState* colorBlendAttachment);
void fr_pso_init_color_blend_attachment_state_blending(VkPipelineColorBlendAttachmentState* colorBlendAttachment);

void fr_pso_init_color_blend_state(VkPipelineColorBlendAttachmentState* colorBlendAttachment,
							  VkPipelineColorBlendStateCreateInfo* colorBlending);

void fr_pso_init_layout(VkDescriptorSetLayout* descriptorSetLayout, VkPipelineLayoutCreateInfo* pipelineLayoutInfo);

void fr_pso_init_depth_stencil_state(VkPipelineDepthStencilStateCreateInfo* depthStencil);

void fr_attachment_init_color(uint32_t attachmentIndex,
							  VkFormat colorFormat,
							  VkAttachmentDescription* colorAttachment,
							  VkAttachmentReference* colorAttachmentRef);

void fr_attachment_init_depth(uint32_t attachmentIndex,
							  VkFormat depthFormat,
							  VkAttachmentDescription* depthAttachment,
							  VkAttachmentReference* depthAttachmentRef);

void fr_subpass_init_color_depth(VkAttachmentReference* colorAttachmentRef, VkAttachmentReference* depthAttachmentRef,
								 VkSubpassDescription* subpass, VkSubpassDependency* dependency);

VkResult fr_render_pass_create_color_depth(VkDevice device, VkFormat colorFormat, VkFormat depthFormat,
										   VkRenderPass* renderPass, fc_alloc_callbacks_t* pAllocCallbacks);
