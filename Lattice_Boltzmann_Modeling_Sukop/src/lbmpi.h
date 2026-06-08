//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// lbmpi.h
//
//  - Note that this is adapted from a previous implementation in an
//    old mcmp code from last summer (2004).
//
#ifndef LBMPI_H
#define LBMPI_H

struct lbmpi_struct
{
  //############################################################################
  //
  // P A R A L L E L   V A R I A B L E S
  //
  //                  1       ...     PX
  //                -------------------------
  //                |       |       |       |
  //              1 |       |       |       |
  //                |       |       |       |
  //                -------------------------
  //              . |       |       |       |
  //              . |       |       |       |
  //              . |       |1 .. LX|       |
  //                -------------------------
  //                |      1|-|-|-|-|       |
  //             PY |    ...|-|-|-|-|       |
  //                |     LY|-|-|-|-|       |
  //                -------------------------
  //
  // 06222004 For now, assume PX (and PY) are evenly divisible by LX (and LY).
  //
  // 06222004 As a first step, assume periodic boundaries everywhere...
  //          (See the TODO file...)
  //

  int NumProcs, // Number of processes.

      ProcID;   // ID of my (this proc's) process.

  // Param: NPX
  // Param: NPY
  // Type: int
  // Comments: NPX is the number of processes in the x-direction. NPY is the
  // number of processes in the y-direction.
  //
  int    NPX,
         NPY;

  // Param: PX
  // Param: PY
  // Type: int
  // Comments: Coordinates of process' subdomain in the 2D array of subdomains.
  //
  int    PX,
         PY;

  // Param: GLX
  // Param: GLY
  // Type: int
  // Comments: GLX-by-GLY is the global domain size (number of lattice nodes in
  // each direction).  This is copied from LX and LY before LX and LY are
  // computed to store the local domain size of each process based on NPX and
  // NPY.
  //
  int    GLX,
         GLY;

  // Param: GSX
  // Param: GSY
  // Param: GEX
  // Param: GEY
  // Type: int
  // Comments: Global starting and ending coordinates of process' piece of
  // the domain. Process owns nodes from (GSX,GSY) to (GEX,GEY).
  //
  int    GSX,
         GSY,
         GEX,
         GEY;

int NorthID; // ID of proc with sub-domain to the north.
int SouthID; // ID of proc with sub-domain to the south.
int EastID;  // ID of proc with sub-domain to the east.
int WestID;  // ID of proc with sub-domain to the west.

char filename[1024];
MPI_Status status;
int ierr;
double tic, toc;

// Need a string for accumulating output to send through printf...
// Outputting elements of an array (e.g. IndicesEW) individually
// will result in a mess under MPI.  Better to output them to a
// string first and then dump the string all at once...  Note that
// this is just for small scale debugging.  For visualizing
// contents of larger arrays, write to a file...
char iobuf[1024];

double sendtmp, recvtmp; // For debugging...

// Stuff for generating type struct for east/west communication.
int *BlockLengthsEW;
MPI_Aint *AddrsEW;
MPI_Aint *IndicesEW;
MPI_Datatype *TypesEW;
MPI_Datatype MPI_West2East;
MPI_Datatype MPI_East2West;

// Stuff for generating type struct for north/south communication.
int *BlockLengthsNS;
MPI_Aint *AddrsNS;
MPI_Aint *IndicesNS;
MPI_Datatype *TypesNS;
MPI_Datatype MPI_South2North;
MPI_Datatype MPI_North2South;

MPI_Aint Index0;

#if DUMP_AFTER_COMM || DUMP_BEFORE_COMM
double **fmat;
#endif /* DUMP_AFTER_COMM || DUMP_BEFORE_COMM */

};
//typedef struct lbmpi_struct *lbmpi_ptr;

#endif LBMPI_H
