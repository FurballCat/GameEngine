#pragma once

namespace gpu
{
    class GraphicsCommandEncoder;
	class ParallelRenderCommandEncoder;
    class BlitCommandEncoder;
    class RenderPassDescriptor;
    class Drawable;
    
    // Lightweight (transient), non-reusable, not thread safe, at any point in time only one command encoder can encode commands into it
    // unless parralel command encoder is used. Is created by CommandQueue.
    // Keeps encoded commands. In simple apps usually one command buffer is enough to encode entire frame.
    // Once committed for execution, the only valid operations are to wait fo it to scheduled or completed and to check status of the execution.
    // Retains references to objects used by command buffer unless command buffer without retaining objects is used (usually critical part of application).
    // Are executed in order in which it they were committed (see CommandQueue). Use Enqueue to reserve place in the queue
    // without actually committing the command yet.
    class GPU_API CommandBuffer
    {
        DECLARE_GPU_PLATFORM_PTR(CommandBuffer, m_ptr);
    public:
        // Create command encoder to write commands into that command buffer. It's lightweight, so I'm not returning pointers here.
        GraphicsCommandEncoder CreateGraphicsCommandEncoder(RenderPassDescriptor* desc);
		
		// Create parallel render command encoder
		ParallelRenderCommandEncoder CreateParallelRenderCommandEncoder(RenderPassDescriptor* desc);
		
        // Create blit command encoder to perform operations on textures like copy, fill etc.
        BlitCommandEncoder CreateBlitCommandEncoder();
        
        // Reserve place in CommandQueue without actually commiting the command.
        void Enqueue();
        
        // Enqueue (if not enqueued previously) and start command execution.
        void Commit();
        
        // Synchronously wait until the command is finished after committing.
        void WaitUntilCompleted();
        
        // Convenience method which schedules a trigger (handler) which will present the contents of a displayable resource (see Drawable).
        void Present(Drawable* drawable);
        
        // todo: add the rest of the methods
    };
    
    // Tips: in case of multithreading rendering you can create multiple command buffers for different threads. If you know the execution order
    // ahead of time, then you can use Enqueue method before sending commands to different threads.
}
