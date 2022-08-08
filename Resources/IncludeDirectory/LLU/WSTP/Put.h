/**
 * @file	Put.h
 * @date	Nov 28, 2017
 * @author	Rafal Chojna <rafalc@wolfram.com>
 * @brief	Header file with classes related to sending data through WSTP.
 */
#ifndef LLU_WSTP_PUT_H_
#define LLU_WSTP_PUT_H_

#include <functional>

#include "wstp.h"

#include "LLU/ErrorLog/Errors.h"
#include "LLU/Utilities.hpp"
#include "LLU/WSTP/Utilities.h"

namespace LLU::WS {

	template<typename T>
	struct PutArray {
		using Func = std::function<int(WSLINK, const T*, const int*, const char**, int)>;

		static void put(WSLINK m, const T* array, const int* dims, const char** heads, int len) {
			Detail::checkError(m, ArrayF(m, array, dims, heads, len), ErrorName::WSPutArrayError, ArrayFName);
		}

		static void put(WSLINK m, const T* array, const int* dims, char** heads, int len) {
			// NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast): ArrayF treats heads as read-only, so this cast is safe
			Detail::checkError(m, ArrayF(m, array, dims, const_cast<const char**>(heads), len), ErrorName::WSPutArrayError, ArrayFName);
		}

	private:
		static const std::string ArrayFName;
		static Func ArrayF;
	};

	template<typename T>
	struct PutList {
		using Func = std::function<int(WSLINK, const T*, int)>;

		static void put(WSLINK m, const T* list, int len) {
			Detail::checkError(m, ListF(m, list, len), ErrorName::WSPutListError, ListFName);
		}

	private:
		static const std::string ListFName;
		static Func ListF;
	};

	template<typename T>
	struct PutScalar {
		using Func = std::function<int(WSLINK, T)>;

		static void put(WSLINK m, T scalar) {
			Detail::checkError(m, ScalarF(m, scalar), ErrorName::WSPutScalarError, ScalarFName);
		}

	private:
		static const std::string ScalarFName;
		static Func ScalarF;
	};

	template<typename T>
	typename PutArray<T>::Func PutArray<T>::ArrayF = [](WSLINK /*link*/, const T* /*values*/, const int* /*dims*/, const char** /*heads*/, int /*length*/) {
		static_assert(dependent_false_v<T>, "Trying to use WS::PutArray<T> for unsupported type T");
		return 0;
	};

	template<typename T>
	typename PutList<T>::Func PutList<T>::ListF = [](WSLINK /*link*/, const T* /*values*/, int /*length*/) {
		static_assert(dependent_false_v<T>, "Trying to use WS::PutList<T> for unsupported type T");
		return 0;
	};

	template<typename T>
	typename PutScalar<T>::Func PutScalar<T>::ScalarF = [](WSLINK /*link*/, T /*value*/) {
		static_assert(dependent_false_v<T>, "Trying to use WS::PutScalar<T> for unsupported type T");
		return 0;
	};

/// @cond
#ifndef _WIN32

#define WS_PUT_DECLARE_SPECIALIZATIONS_OF_STATIC_MEMBERS(T) \
	template<>                                              \
	PutArray<T>::Func PutArray<T>::ArrayF;                  \
	template<>                                              \
	const std::string PutArray<T>::ArrayFName;              \
	template<>                                              \
	PutList<T>::Func PutList<T>::ListF;                     \
	template<>                                              \
	const std::string PutList<T>::ListFName;                \
	template<>                                              \
	PutScalar<T>::Func PutScalar<T>::ScalarF;               \
	template<>                                              \
	const std::string PutScalar<T>::ScalarFName;

	WS_PUT_DECLARE_SPECIALIZATIONS_OF_STATIC_MEMBERS(unsigned char)
	WS_PUT_DECLARE_SPECIALIZATIONS_OF_STATIC_MEMBERS(short)
	WS_PUT_DECLARE_SPECIALIZATIONS_OF_STATIC_MEMBERS(int)
	WS_PUT_DECLARE_SPECIALIZATIONS_OF_STATIC_MEMBERS(wsint64)
	WS_PUT_DECLARE_SPECIALIZATIONS_OF_STATIC_MEMBERS(float)
	WS_PUT_DECLARE_SPECIALIZATIONS_OF_STATIC_MEMBERS(double)

#else

	/* ***************************************************************** */
	/* ********* Template specializations for  unsigned char  ********** */
	/* ***************************************************************** */

	/* PutArray */

	template<>
	PutArray<unsigned char>::Func PutArray<unsigned char>::ArrayF = WSPutInteger8Array;

	template<>
	const std::string PutArray<unsigned char>::ArrayFName = "WSPutInteger8Array";

	/* PutList */

	template<>
	PutList<unsigned char>::Func PutList<unsigned char>::ListF = WSPutInteger8List;

	template<>
	const std::string PutList<unsigned char>::ListFName = "WSPutInteger8List";

	/* PutScalar */

	template<>
	PutScalar<unsigned char>::Func PutScalar<unsigned char>::ScalarF = WSPutInteger8;

	template<>
	const std::string PutScalar<unsigned char>::ScalarFName = "WSPutInteger8";

	/* ***************************************************************** */
	/* ******* Template specializations for  (unsigned) short  ********* */
	/* ***************************************************************** */

	/* PutArray */

	template<>
	PutArray<short>::Func PutArray<short>::ArrayF = WSPutInteger16Array;

	template<>
	const std::string PutArray<short>::ArrayFName = "WSPutInteger16Array";

	/* PutList */

	template<>
	PutList<short>::Func PutList<short>::ListF = WSPutInteger16List;

	template<>
	const std::string PutList<short>::ListFName = "WSPutInteger16List";

	/* PutScalar */

	template<>
	PutScalar<short>::Func PutScalar<short>::ScalarF = WSPutInteger16;

	template<>
	const std::string PutScalar<short>::ScalarFName = "WSPutInteger16";

	/* ***************************************************************** */
	/* ******** Template specializations for  (unsigned) int  ********** */
	/* ***************************************************************** */

	/* PutArray */

	template<>
	PutArray<int>::Func PutArray<int>::ArrayF = WSPutInteger32Array;

	template<>
	const std::string PutArray<int>::ArrayFName = "WSPutInteger32Array";

	/* PutList */

	template<>
	PutList<int>::Func PutList<int>::ListF = WSPutInteger32List;

	template<>
	const std::string PutList<int>::ListFName = "WSPutInteger32List";

	/* PutScalar */

	template<>
	PutScalar<int>::Func PutScalar<int>::ScalarF = WSPutInteger32;

	template<>
	const std::string PutScalar<int>::ScalarFName = "WSPutInteger32";

	/* ***************************************************************** */
	/* *********** Template specializations for  wsint64  ************** */
	/* ***************************************************************** */

	/* PutArray */

	template<>
	PutArray<wsint64>::Func PutArray<wsint64>::ArrayF = WSPutInteger64Array;

	template<>
	const std::string PutArray<wsint64>::ArrayFName = "WSPutInteger64Array";

	/* PutList */

	template<>
	PutList<wsint64>::Func PutList<wsint64>::ListF = WSPutInteger64List;

	template<>
	const std::string PutList<wsint64>::ListFName = "WSPutInteger64List";

	/* PutScalar */

	template<>
	PutScalar<wsint64>::Func PutScalar<wsint64>::ScalarF = WSPutInteger64;

	template<>
	const std::string PutScalar<wsint64>::ScalarFName = "WSPutInteger64";

	/* ***************************************************************** */
	/* ************ Template specializations for  float  *************** */
	/* ***************************************************************** */

	/* PutArray */

	template<>
	PutArray<float>::Func PutArray<float>::ArrayF = WSPutReal32Array;

	template<>
	const std::string PutArray<float>::ArrayFName = "WSPutReal32Array";

	/* PutList */

	template<>
	PutList<float>::Func PutList<float>::ListF = WSPutReal32List;

	template<>
	const std::string PutList<float>::ListFName = "WSPutReal32List";

	/* PutScalar */

	template<>
	PutScalar<float>::Func PutScalar<float>::ScalarF = WSPutReal32;

	template<>
	const std::string PutScalar<float>::ScalarFName = "WSPutReal32";

	/* ***************************************************************** */
	/* *********** Template specializations for  double  *************** */
	/* ***************************************************************** */

	/* PutArray */

	template<>
	PutArray<double>::Func PutArray<double>::ArrayF = WSPutReal64Array;

	template<>
	const std::string PutArray<double>::ArrayFName = "WSPutReal64Array";

	/* PutList */

	template<>
	PutList<double>::Func PutList<double>::ListF = WSPutReal64List;

	template<>
	const std::string PutList<double>::ListFName = "WSPutReal64List";

	/* PutScalar */

	template<>
	PutScalar<double>::Func PutScalar<double>::ScalarF = WSPutReal64;

	template<>
	const std::string PutScalar<double>::ScalarFName = "WSPutReal64";

#endif
/// @endcond

} /* namespace LLU::WS */

#endif /* LLU_WSTP_PUT_H_ */
