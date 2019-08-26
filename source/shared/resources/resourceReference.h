#pragma once

namespace res
{
	class Resource;
	
	class RESOURCES_API ResourceRefBase
	{
	protected:
		Resource* LoadResource();
		
		ResourcePath m_path;
	};
	
	template<typename T>
	class RESOURCES_API ResourceRef : public ResourceRefBase
	{
	public:
		ResourceRef();
		ResourceRef(const ResourcePath& path);
		
		T* Get()
		{
			if(m_resource)
				return m_resource;
			
			m_resource = LoadResource();
			return m_resource;
		}
		
	private:
		T* m_resource = nullptr;
	};
}
