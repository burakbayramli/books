//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// lbio.h
//
//  - Header file for IO stuff.
//
//  - Mainly, a structure containing paths to input and output files
//    and routines for reading these paths from a file or command line
//    or environment variables...
//
//  - This stuff is in the global scope, included from lb2d_prime.h.
//

struct io_struct
{
  char *in_path;
  char *out_path;
  char *subs00_path;
  char *subs01_path;
};
typedef struct io_struct *io_ptr;

//
