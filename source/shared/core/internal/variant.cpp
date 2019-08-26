#include "pch.h"
#include "variant.h"

Variant& Variant::operator=(const Variant& other)
{
	m_type = other.m_type;
	m_data = other.m_data;
	return *this;
}

Variant& Variant::operator+=(const Variant& b)
{
	ASSERT(m_type == b.m_type);
	switch (m_type)
	{
	case VariantType::VarFloat:
		m_asFloat += b.m_asFloat;
		break;
	case VariantType::VarInteger:
		m_asInteger += b.m_asInteger;
	default:
		ASSERT(false);
	}

	return *this;
}

Variant& Variant::operator-=(const Variant& b)
{
	ASSERT(m_type == b.m_type);
	switch (m_type)
	{
	case VariantType::VarFloat:
		m_asFloat -= b.m_asFloat;
		break;
	case VariantType::VarInteger:
		m_asInteger -= b.m_asInteger;
	default:
		ASSERT(false);
	}

	return *this;
}

Variant& Variant::operator*=(const Variant& b)
{
	ASSERT(m_type == b.m_type);
	switch (m_type)
	{
	case VariantType::VarFloat:
		m_asFloat *= b.m_asFloat;
		break;
	case VariantType::VarInteger:
		m_asInteger *= b.m_asInteger;
	default:
		ASSERT(false);
	}

	return *this;
}

Variant& Variant::operator/=(const Variant& b)
{
	ASSERT(m_type == b.m_type);
	switch (m_type)
	{
	case VariantType::VarFloat:
		m_asFloat /= b.m_asFloat;
		break;
	case VariantType::VarInteger:
		m_asInteger /= b.m_asInteger;
	default:
		ASSERT(false);
	}

	return *this;
}

Variant operator+(const Variant& a, const Variant& b)
{
	ASSERT(a.GetVarType() == b.GetVarType());
	switch (a.GetVarType())
	{
	case VariantType::VarFloat:
		return Variant(a.AsFloat() + b.AsFloat());
	case VariantType::VarInteger:
		return Variant(a.AsInteger() + b.AsInteger());
	default:
		ASSERT(false);
	}

	return Variant(0);
}

Variant operator-(const Variant& a, const Variant& b)
{
	ASSERT(a.GetVarType() == b.GetVarType());
	switch (a.GetVarType())
	{
	case VariantType::VarFloat:
		return Variant(a.AsFloat() - b.AsFloat());
	case VariantType::VarInteger:
		return Variant(a.AsInteger() - b.AsInteger());
	default:
		ASSERT(false);
	}

	return Variant(0);
}

Variant operator*(const Variant& a, const Variant& b)
{
	ASSERT(a.GetVarType() == b.GetVarType());
	switch (a.GetVarType())
	{
	case VariantType::VarFloat:
		return Variant(a.AsFloat() * b.AsFloat());
	case VariantType::VarInteger:
		return Variant(a.AsInteger() * b.AsInteger());
	default:
		ASSERT(false);
	}

	return Variant(0);
}

Variant operator/(const Variant& a, const Variant& b)
{
	ASSERT(a.GetVarType() == b.GetVarType());
	switch (a.GetVarType())
	{
	case VariantType::VarFloat:
		return Variant(a.AsFloat() / b.AsFloat());
	case VariantType::VarInteger:
		return Variant(a.AsInteger() / b.AsInteger());
	default:
		ASSERT(false);
	}

	return Variant(0);
}
