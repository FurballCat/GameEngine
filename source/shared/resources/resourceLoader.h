#pragma once

namespace res
{
	struct LoadedResourceEntry
	{
		Resource* m_resource;
		uint32 m_refCount = 0;
	};
	
	class RESOURCES_API ResourceLoader
	{
	public:
		void MountDirectory(const AbsolutePath& path, Name mappingName);
		// todo: MountBundle etc. in future
		
		Resource* LoadResource(const ResourcePath& path);
		
	private:
		DynArray<Name> m_mappingNames;
		DynArray<AbsolutePath> m_mappingPaths;
		
		HashMap<Name, LoadedResourceEntry> m_loadedResources;
	};
}
