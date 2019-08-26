#pragma once

namespace rend
{
	struct Mesh;
	
	// todo: refactor this out
	struct OneObjectInfo
	{
		math::Vector4 m_position;
		math::Vector4 m_color;
	};
	
	// todo: refactor this out
	struct ConstantBufferData
	{
		OneObjectInfo m_objects[40];
	};
	
	struct Camera
	{
		math::Vector3 m_eye;
		math::Vector3 m_at;
		math::Vector3 m_up;
		float m_fov;
	};
	
	struct TileInfo
	{
		uint32 m_x;
		uint32 m_y;
		uint16 m_visualIndex;
	};
	
	// todo: maybe move frame structure somewhere out of renderer
	struct Frame
	{
		Camera m_camera;
		
		float m_rotX;
		float m_rotY;
		
		ConstantBufferData* m_objectsData = nullptr;
		uint32 m_objectsCount = 0;
		ArrayView<TileInfo> m_tiles;
		
		// test mesh
		Mesh* m_testMesh = nullptr;
		gpu::Buffer* m_testInstanceMatrices = nullptr;
		gpu::Buffer* m_testInstanceColors = nullptr;
		gpu::Texture* m_testAlbedoTexture = nullptr;
		gpu::Texture* m_testAmbientOcclusionTexture = nullptr;
		uint32 m_testNumInstances = 0;
		
		// test sprites
		gpu::Buffer* m_testSpriteInstances = nullptr;
		uint32 m_testNumSpriteInstances = 0;
	};
}
