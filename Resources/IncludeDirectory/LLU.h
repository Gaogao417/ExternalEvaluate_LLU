#include <LLU/LLU.h>
#include "WolframCompileLibrary.h"
DLLEXPORT mint WolframLibrary_getVersion () {
       return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize (WolframLibraryData libData)
{
       LLU::LibraryData::setLibraryData (libData);
       return 0;
}

DLLEXPORT void WolframLibrary_uninitialize (WolframLibraryData libData) {
       return;
}