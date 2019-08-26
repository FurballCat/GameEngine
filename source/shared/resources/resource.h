#pragma once

namespace res
{
	struct SerializerData;
	
	class RESOURCES_API Resource
	{
	public:
		virtual void Serialize(SerializerData& serializer) = 0;
	};
}
