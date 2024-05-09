#include <stdio.h>
#include <mpi.h>
#include <string.h>

int main(int argc, char **argv) {
  char mpiVersionString[MPI_MAX_LIBRARY_VERSION_STRING];
  char *beginPtr, *endPtr, *searchPtr;

  int returnVal, resultLength;

  returnVal = MPI_Get_library_version(mpiVersionString, &resultLength) ;
  if( returnVal != MPI_SUCCESS ) {
    puts("Unknown MPI");
  } else   {
    endPtr = strchr(mpiVersionString, '\n');
    beginPtr = mpiVersionString;
    if( (searchPtr = strstr(mpiVersionString, ": CRAY ")) ){ // Deliberate use of '=' here
      beginPtr = searchPtr + 2;
    }
    if( (searchPtr = strstr(mpiVersionString, "Open MPI ")) ){ // Deliberate use of '=' here
      endPtr = strchr(mpiVersionString, ',');
    }
    *endPtr = '\0';
    puts(beginPtr);
  }
}
