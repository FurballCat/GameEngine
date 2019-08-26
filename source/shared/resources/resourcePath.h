#pragma once

namespace res
{
	class RESOURCES_API ResourcePath
	{
	public:
		ResourcePath(const char* path);
		
		// todo: ifdef this
		const char* AsDebugString() const { return m_path.AsText(); }
		
	private:
		Name m_path;			// for example: "content/characters/ann.go" or "test/levels/island.lvl"
		Name m_rootDirectory;	// for example: "content" or "test"
	};
}
