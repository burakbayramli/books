// Connect2D.m
// function [EToE, EToF] = Connect2D(EToV)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include "CS_Type.h"

// #include "Stopwatch.h"
// stopwatch timer2;

//---------------------------------------------------------
void Connect2D
(
  const IMat& EToV,
        IMat& EToE, 
        IMat& EToF
)
//---------------------------------------------------------
{
  // function [EToE, EToF] = Connect2D(EToV)
  // Purpose  : Build global connectivity arrays for grid based on
  //            standard EToV input array from grid generator

  int Nfaces = 3;

  // Find number of elements and vertices
  int K = EToV.num_rows(), Nv = EToV.max_val();

  // Create face to node connectivity matrix
  int TotalFaces = Nfaces*K;

  // List of local face to local vertex connections
  IMat vn(gRowData, 3,2, "1 2  2 3  1 3");

  // Build global face to node sparse array
  CSi SpFToV(TotalFaces, Nv, 2*TotalFaces, 1, 1);
  CSi SpFToF, II2;

  II2.identity(TotalFaces); II2 *= 2;   // II2.print(false);

  int sk = 1;
  for (int k=1; k<=K; ++k) {
    for (int face=1; face<=Nfaces; ++face) 
    {
      SpFToV.set1(sk, EToV(k, vn(face,1)), 1);
      SpFToV.set1(sk, EToV(k, vn(face,2)), 1);
      ++sk;
    }
  }

//FILE* fp1=fopen("FToV_tri.dat", "w");
//SpFToV.write_ML(fp1); fclose(fp1);
  
  SpFToV.compress();
  
//FILE* fp2=fopen("FToV_csc.dat", "w");
//SpFToV.write_ML(fp2); fclose(fp2);


  // Build global face to global face sparse array
  SpFToF = SpFToV*trans(SpFToV) - II2;

//FILE* fp1=fopen("nnsp.dat", "w");
//SpFToF.write_ML(fp1);
//fclose(fp1);


  // Find complete face to face connections
  IMat F12 = SpFToF.find2D('=', 2);
  IVec faces1=F12(All,1), faces2=F12(All,2);

#if (0)
  dumpIMat(F12, "F12");
  dumpIVec(faces1, "faces1");
  dumpIVec(faces2, "faces2");
#endif

  // Convert face global number to element and face numbers
  IVec element1 = floor( (faces1-1)/ Nfaces ) + 1;
  IVec face1    =   mod( (faces1-1), Nfaces ) + 1;
  IVec element2 = floor( (faces2-1)/ Nfaces ) + 1;
  IVec face2    =   mod( (faces2-1), Nfaces ) + 1;

  // Rearrange into Nelements x Nfaces sized arrays
  IVec ind = sub2ind(K, Nfaces, element1, face1);

  EToE = outer(Range(1,K), Ones(Nfaces));
  EToF = outer(Ones(K), Range(1,Nfaces));

  EToE(ind) = element2;
  EToF(ind) = face2;

#if (0)
  dumpIMat(EToE, "EToE Connect2D");
  dumpIMat(EToF, "EToF Connect2D");
  umERROR("Testing", "Nigel, check arrays");
#endif
}
