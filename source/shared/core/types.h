#pragma once

#include <cassert>
#include <algorithm>
#include <type_traits>
#include <atomic>
#include <thread>
#include <memory>
#include <future>
#include <mutex>
#include <chrono>
#include <random>
#include <string>
#include <vector>
#include <thread>
#include <queue>
#include <stack>
#include <list>
#include <array>
#include <map>
#include <unordered_map>
#include <sstream>
#include <fstream>
#include <streambuf>

template<class T> using List = std::list<T>;
template<class T> using Queue = std::queue<T>;
template<class T> using Deque = std::deque<T>;
template<class T> using Stack = std::stack<T>;
template<class T> using DynArray = std::vector<T>;
template<class T, class T1> using Map = std::map<T, T1>;
template<typename T, size_t size> using Array = std::array<T, size>;

template<class T> using WeakPtr = std::weak_ptr<T>;
template<class T> using SharedPtr = std::shared_ptr<T>;
template<class T> using UniquePtr = std::unique_ptr<T>;
template<class T> using SmartComPtr = std::unique_ptr<T, void( *)( T* )>;

using int8 = std::int8_t;
using int16 = std::int16_t;
using int32 = std::int32_t;
using int64 = std::int64_t;

using uint8 = std::uint8_t;
using uint16 = std::uint16_t;
using uint32 = std::uint32_t;
using uint64 = std::uint64_t;

using uintptr = std::uintptr_t;

static_assert(sizeof(int8) == 1, "");
static_assert(sizeof(int16) == 2, "");
static_assert(sizeof(int32) == 4, "");
static_assert(sizeof(int64) == 8, "");

static_assert(sizeof(uint8) == 1, "");
static_assert(sizeof(uint16) == 2, "");
static_assert(sizeof(uint32) == 4, "");
static_assert(sizeof(uint64) == 8, "");

using String = std::string;
using StringStream = std::stringstream;

using InputStream = std::istream;
using FileStream = std::fstream;

template<typename TKey, typename TValue>
using HashMap = std::unordered_map<TKey, TValue>;

namespace fur
{
	template<typename T>
	using Atomic = std::atomic<T>;
}

using ResourceID = uint64;
using StringHash = uint32;

#ifdef PLATFORM_OSX
	#define FUR_LOG_ERROR(_text) printf("%s", _text);
#elif PLATFORM_WINDOWS
	#define FUR_LOG_ERROR(_text)
#else
	#define FUR_LOG_ERROR(_text)
#endif

#define FUR_UNUSED(_x) (void)(_x)
