#pragma once

namespace gpu
{
    struct GPU_API BufferDesc
    {
		BufferDesc() : m_data(nullptr), m_size(0) {}
		BufferDesc(const void* data, uint32 size, ResourceOptions options) : m_data(data), m_size(size), m_options(options) {}
		
        const void* m_data;
        uint32 m_size;
		ResourceOptions m_options = ResourceOptions::CpuCacheModeDefaultCache;
    };
    
    // Heavy, non-thread-safe, resource used to store any type of data, for example vertices, shader state objects like constant buffers etc.
    // It's basically a block of memory that can contain any type of data. Created by device.
    // Buffers are attached to slots in shaders, then shader program is accessing it through slots and interpreting it's content.
    class GPU_API Buffer
    {
        DECLARE_GPU_PLATFORM_PTR(Buffer, m_ptr);
    public:
		Buffer() {}
		
        // Returns size of the content in bytes.
        uint32 GetSize() const;
        
        // Get content for modification. Be aware, that if you want to update specific buffer for different draw calls then its content
        // will not be preserved for previous draw calls unless command buffers which contains them were committed. Imagine it like that:
        // every command buffer holds a reference to the same memory. After command buffer is comitted the data is copied, so you are again
        // free to change the content.
        void* GetContent();
    };
}
