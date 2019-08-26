#pragma once

#ifdef PLATFORM_OSX
    #define GPU_METAL_ENUM_VALUE(_value) = (int32)_value
    #define GPU_DIRECTX_ENUM_VALUE(_value)
#elif PLATFORM_WINDOWS
    #define GPU_METAL_ENUM_VALUE(_value)
    #define GPU_DIRECTX_ENUM_VALUE(_value) = (int32)_value
#endif

#ifdef PLATFORM_OSX
    #define GPU_METAL_NATIVE_ENUM_MAPPING(_enum, _nativeEnum) inline _nativeEnum ToNativeEnum( _enum value ) { return (_nativeEnum)value; }
    #define GPU_DIRECTX_NATIVE_ENUM_MAPPING(_enum, _nativeEnum)
#elif PLATFORM_WINDOWS
    #define GPU_METAL_NATIVE_ENUM_MAPPING(_enum, _nativeEnum)
    #define GPU_DIRECTX_NATIVE_ENUM_MAPPING(_enum, _nativeEnum) inline _nativeEnum ToNativeEnum( _enum value ) { return (_nativeEnum)value; }
#endif

namespace gpu
{
    // todo: add DirectX equivalents in places of zeros in GPU_METAL_ENUM_VALUE
    
    enum class PixelFormat
    {
        Invalid                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::Invalid)   GPU_DIRECTX_ENUM_VALUE(0),
        
        A8Unorm                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::A8Unorm),
        
        R8Unorm                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R8Unorm),
        R8Unorm_sRGB            GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R8Unorm_sRGB),
        
        R8Snorm                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R8Snorm),
        R8Uint                  GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R8Uint),
        R8Sint                  GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R8Sint),
        
        R16Unorm                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R16Unorm),
        R16Snorm                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R16Snorm),
        R16Uint                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R16Uint),
        R16Sint                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R16Sint),
        R16Float                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R16Float),
        
        RG8Unorm                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG8Unorm),
        RG8Unorm_sRGB           GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG8Unorm_sRGB),
        RG8Snorm                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG8Snorm),
        RG8Uint                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG8Uint),
        RG8Sint                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG8Sint),
        
        B5G6R5Unorm             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::B5G6R5Unorm),
        A1BGR5Unorm             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::A1BGR5Unorm),
        ABGR4Unorm              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::ABGR4Unorm),
        BGR5A1Unorm             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BGR5A1Unorm),
        
        R32Uint                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R32Uint),
        R32Sint                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R32Sint),
        R32Float                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::R32Float),
        
        RG16Unorm               GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG16Unorm),
        RG16Snorm               GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG16Snorm),
        RG16Uint                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG16Uint),
        RG16Sint                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG16Sint),
        RG16Float               GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG16Float),
        
        RGBA8Unorm              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA8Unorm),
        RGBA8Unorm_sRGB         GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA8Unorm_sRGB),
        RGBA8Snorm              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA8Snorm),
        RGBA8Uint               GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA8Uint),
        RGBA8Sint               GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA8Sint),
        
        BGRA8Unorm              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BGRA8Unorm),
        BGRA8Unorm_sRGB         GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BGRA8Unorm_sRGB),
        
        RGB10A2Unorm            GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGB10A2Unorm),
        RGB10A2Uint             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGB10A2Uint),
        
        RG11B10Float            GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG11B10Float),
        RGB9E5Float             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGB9E5Float),
        
        BGR10_XR                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BGR10_XR),
        BGR10_XR_sRGB           GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BGR10_XR_sRGB),
        
        RG32Uint                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG32Uint),
        RG32Sint                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG32Sint),
        RG32Float               GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RG32Float),
        
        RGBA16Unorm             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA16Unorm),
        RGBA16Snorm             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA16Snorm),
        RGBA16Uint              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA16Uint),
        RGBA16Sint              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA16Sint),
        RGBA16Float             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA16Float),
        
        BGRA10_XR               GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BGRA10_XR),
        BGRA10_XR_sRGB          GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BGRA10_XR_sRGB),
        
        RGBA32Uint              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA32Uint),
        RGBA32Sint              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA32Sint),
        RGBA32Float             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::RGBA32Float),
        
        BC1_RGBA                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC1_RGBA),
        BC1_RGBA_sRGB           GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC1_RGBA_sRGB),
        BC2_RGBA                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC2_RGBA),
        BC2_RGBA_sRGB           GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC2_RGBA_sRGB),
        BC3_RGBA                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC3_RGBA),
        BC3_RGBA_sRGB           GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC3_RGBA_sRGB),
        
        BC4_RUnorm              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC4_RUnorm),
        BC4_RSnorm              GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC4_RSnorm),
        BC5_RGUnorm             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC5_RGUnorm),
        BC5_RGSnorm             GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC5_RGSnorm),
        
        BC6H_RGBFloat           GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC6H_RGBFloat),
        BC6H_RGBUfloat          GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC6H_RGBUfloat),
        BC7_RGBAUnorm           GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC7_RGBAUnorm),
        BC7_RGBAUnorm_sRGB      GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BC7_RGBAUnorm_sRGB),

        GBGR422                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::GBGR422),
        
        BGRG422                 GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::BGRG422),
        
        Depth16Unorm            GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::Depth16Unorm),
        Depth32Float            GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::Depth32Float),
        
        Stencil8                GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::Stencil8),
        
        Depth24Unorm_Stencil8   GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::Depth24Unorm_Stencil8),
        Depth32Float_Stencil8   GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::Depth32Float_Stencil8),
        
        X32_Stencil8            GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::X32_Stencil8),
        X24_Stencil8            GPU_METAL_ENUM_VALUE(mtlpp::PixelFormat::X24_Stencil8),
    };
    
    GPU_METAL_NATIVE_ENUM_MAPPING(PixelFormat, mtlpp::PixelFormat);
    
    enum class TextureUsage
    {
        Unknown                 GPU_METAL_ENUM_VALUE(mtlpp::TextureUsage::Unknown),
        ShaderRead              GPU_METAL_ENUM_VALUE(mtlpp::TextureUsage::ShaderRead),
        ShaderWrite             GPU_METAL_ENUM_VALUE(mtlpp::TextureUsage::ShaderWrite),
        RenderTarget            GPU_METAL_ENUM_VALUE(mtlpp::TextureUsage::RenderTarget),
        PixelFormatView         GPU_METAL_ENUM_VALUE(mtlpp::TextureUsage::PixelFormatView),
    };
    
    GPU_METAL_NATIVE_ENUM_MAPPING(TextureUsage, mtlpp::TextureUsage);
	
	enum class StorageMode
	{
		Shared			GPU_METAL_ENUM_VALUE(mtlpp::StorageMode::Shared),
		Managed			GPU_METAL_ENUM_VALUE(mtlpp::StorageMode::Managed),
		Private			GPU_METAL_ENUM_VALUE(mtlpp::StorageMode::Private),
		Memoryless		GPU_METAL_ENUM_VALUE(mtlpp::StorageMode::Memoryless),
	};
	
	GPU_METAL_NATIVE_ENUM_MAPPING(StorageMode, mtlpp::StorageMode);
	
    enum class PrimitiveType
    {
        Point               GPU_METAL_ENUM_VALUE(mtlpp::PrimitiveType::Point),
        Line                GPU_METAL_ENUM_VALUE(mtlpp::PrimitiveType::Line),
        LineStrip           GPU_METAL_ENUM_VALUE(mtlpp::PrimitiveType::LineStrip),
        Triangle            GPU_METAL_ENUM_VALUE(mtlpp::PrimitiveType::Triangle),
        TriangleStrip       GPU_METAL_ENUM_VALUE(mtlpp::PrimitiveType::TriangleStrip),
    };
    
    GPU_METAL_NATIVE_ENUM_MAPPING(PrimitiveType, mtlpp::PrimitiveType);
    
    enum class LoadAction
    {
        DontCare        GPU_METAL_ENUM_VALUE(mtlpp::LoadAction::DontCare),
        Load            GPU_METAL_ENUM_VALUE(mtlpp::LoadAction::Load),
        Clear           GPU_METAL_ENUM_VALUE(mtlpp::LoadAction::Clear),
    };
    
    GPU_METAL_NATIVE_ENUM_MAPPING(LoadAction, mtlpp::LoadAction);
    
    enum class StoreAction
    {
        DontCare                        GPU_METAL_ENUM_VALUE(mtlpp::StoreAction::DontCare),
        Store                           GPU_METAL_ENUM_VALUE(mtlpp::StoreAction::Store),
        MultisampleResolve              GPU_METAL_ENUM_VALUE(mtlpp::StoreAction::MultisampleResolve),
        StoreAndMultisampleResolve      GPU_METAL_ENUM_VALUE(mtlpp::StoreAction::StoreAndMultisampleResolve),
        Unknown                         GPU_METAL_ENUM_VALUE(mtlpp::StoreAction::Unknown),
    };
    
    GPU_METAL_NATIVE_ENUM_MAPPING(StoreAction, mtlpp::StoreAction);
	
	enum class VertexFormat
	{
		Invalid						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Invalid),
		
		UChar2						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UChar2),
		UChar3						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UChar3),
		UChar4						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UChar4),
		
		Char2						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Char2),
		Char3						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Char3),
		Char4						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Char4),
		
		UChar2Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UChar2Normalized),
		UChar3Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UChar3Normalized),
		UChar4Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UChar4Normalized),
		
		Char2Normalized				GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Char2Normalized),
		Char3Normalized				GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Char3Normalized),
		Char4Normalized				GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Char4Normalized),
		
		UShort2						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UShort2),
		UShort3						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UShort3),
		UShort4						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UShort4),
		
		Short2						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Short2),
		Short3						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Short3),
		Short4						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Short4),
		
		UShort2Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UShort2Normalized),
		UShort3Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UShort3Normalized),
		UShort4Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UShort4Normalized),
		
		Short2Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Short2Normalized),
		Short3Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Short3Normalized),
		Short4Normalized			GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Short4Normalized),
		
		Half2						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Half2),
		Half3						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Half3),
		Half4						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Half4),
		
		Float						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Float),
		Float2						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Float2),
		Float3						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Float3),
		Float4						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Float4),
		
		Int							GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Int),
		Int2						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Int2),
		Int3						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Int3),
		Int4						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Int4),
		
		UInt						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UInt),
		UInt2						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UInt2),
		UInt3						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UInt3),
		UInt4						GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UInt4),
		
		Int1010102Normalized		GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::Int1010102Normalized),
		UInt1010102Normalized		GPU_METAL_ENUM_VALUE(mtlpp::VertexFormat::UInt1010102Normalized),
	};
	
	GPU_METAL_NATIVE_ENUM_MAPPING(VertexFormat, mtlpp::VertexFormat);
	
	enum class VertexStepFunction
	{
		Constant				GPU_METAL_ENUM_VALUE(mtlpp::VertexStepFunction::Constant),
		PerVertex				GPU_METAL_ENUM_VALUE(mtlpp::VertexStepFunction::PerVertex),
		PerInstance				GPU_METAL_ENUM_VALUE(mtlpp::VertexStepFunction::PerInstance),
		PerPatch				GPU_METAL_ENUM_VALUE(mtlpp::VertexStepFunction::PerPatch),
		PerPatchControlPoint	GPU_METAL_ENUM_VALUE(mtlpp::VertexStepFunction::PerPatchControlPoint),
	};
	
	GPU_METAL_NATIVE_ENUM_MAPPING(VertexStepFunction, mtlpp::VertexStepFunction);
	
	enum class CompareFunction
	{
		Never        GPU_METAL_ENUM_VALUE(mtlpp::CompareFunction::Never),
		Less         GPU_METAL_ENUM_VALUE(mtlpp::CompareFunction::Less),
		Equal        GPU_METAL_ENUM_VALUE(mtlpp::CompareFunction::Equal),
		LessEqual    GPU_METAL_ENUM_VALUE(mtlpp::CompareFunction::LessEqual),
		Greater      GPU_METAL_ENUM_VALUE(mtlpp::CompareFunction::Greater),
		NotEqual     GPU_METAL_ENUM_VALUE(mtlpp::CompareFunction::NotEqual),
		GreaterEqual GPU_METAL_ENUM_VALUE(mtlpp::CompareFunction::GreaterEqual),
		Always       GPU_METAL_ENUM_VALUE(mtlpp::CompareFunction::Always),
	};
	
	GPU_METAL_NATIVE_ENUM_MAPPING(CompareFunction, mtlpp::CompareFunction);
	
	enum class ResourceOptions
	{
		CpuCacheModeDefaultCache		GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::CpuCacheModeDefaultCache),
		CpuCacheModeWriteCombined		GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::CpuCacheModeWriteCombined),
		StorageModeShared				GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::StorageModeShared),
		StorageModeManaged				GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::StorageModeManaged),
		StorageModePrivate				GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::StorageModePrivate),
		StorageModeMemoryless			GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::StorageModeMemoryless),
		HazardTrackingModeUntracked		GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::HazardTrackingModeUntracked),
		OptionCpuCacheModeDefault       GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::OptionCpuCacheModeDefault),
		OptionCpuCacheModeWriteCombined	GPU_METAL_ENUM_VALUE(mtlpp::ResourceOptions::OptionCpuCacheModeWriteCombined),
	};
	
	GPU_METAL_NATIVE_ENUM_MAPPING(ResourceOptions, mtlpp::ResourceOptions);
}
