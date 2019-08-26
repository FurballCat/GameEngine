#pragma once

namespace rend
{
	// Potentially visible set of renderable objects
	struct PotentiallyVisibleSet
	{
		DynArray<Mesh*> m_meshes;
	};
}
