#pragma once

namespace res
{
	struct SerializerData
	{
		int32 m_version = -1;
		FileStream m_file;
		bool m_isWriting = false;
	};
	
	template<typename _pod>
	void Serialize(SerializerData& serializer, _pod* datum)
	{
		static_assert(std::is_pod<_pod>::value, "Can't use POD serialization when type is not POD.");
		
		if(serializer.m_isWriting)
		{
			serializer.m_file.read(&datum, sizeof(_pod));
		}
		else
		{
			serializer.m_file.write(&datum, sizeof(_pod));
		}
	}
}
