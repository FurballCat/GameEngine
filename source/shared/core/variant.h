#pragma once

enum class VariantType
{
	VarFloat,
	VarInteger,
	VarBool,
	//VarName,
};

class CORE_API Variant
{
public:
	Variant(float value) : m_type(VariantType::VarFloat), m_asFloat(value) {}
	Variant(int32 value) : m_type(VariantType::VarInteger), m_asInteger(value) {}
	Variant(bool value) : m_type(VariantType::VarBool), m_asBool(value) {}
	//Variant(Name value) : m_type(VariantType::VarName), m_asName(value) {}

	~Variant() {}

	Variant& operator=(const Variant& other);
	Variant& operator+=(const Variant& b);
	Variant& operator-=(const Variant& b);
	Variant& operator*=(const Variant& b);
	Variant& operator/=(const Variant& b);

	VariantType GetVarType() const
	{
		return m_type;
	}

	float& AsFloat() { return m_asFloat; }
	int32& AsInteger() { return m_asInteger; }
	bool& AsBool() { return m_asBool; }
	//Name& AsName() { return m_asName; }

	float AsFloat() const { return m_asFloat; }
	int32 AsInteger() const { return m_asInteger; }
	bool AsBool() const { return m_asBool; }
	//Name AsName() const { return m_asName; }

private:
	VariantType m_type;

	union
	{
		uint32 m_data;
		float m_asFloat;
		int32 m_asInteger;
		bool m_asBool;
		//Name m_asName;		// TODO: Name should be only a hash (U32), it's not right now
	};
};

Variant operator+(const Variant& a, const Variant& b);
Variant operator-(const Variant& a, const Variant& b);
Variant operator*(const Variant& a, const Variant& b);
Variant operator/(const Variant& a, const Variant& b);
