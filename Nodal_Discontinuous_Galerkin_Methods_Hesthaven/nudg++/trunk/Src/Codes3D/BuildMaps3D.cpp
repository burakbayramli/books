// BuildMaps3D.m
// function [vmapM, vmapP, vmapB, mapB] = BuildMaps3D
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"

//---------------------------------------------------------
void NDG3D::BuildMaps3D()
//---------------------------------------------------------
{
  // function [vmapM, vmapP, vmapB, mapB] = BuildMaps3D
  // Purpose: Connectivity and boundary tables for nodes given
  // 	   in the K # of elements, each with N+1 degrees of freedom.

  // Find node to node connectivity

  IVec idsL, idsR, idsM,idsP, vidM,vidP, idM,idP;
  IMat idMP; DMat xM,xP,yM,yP,zM,zP,D;  DVec x1,x2,y1,y2,z1,z2;
  int k1=0,f1=0, k2=0,f2=0, skL=0,skR=0, skM=0,skP=0, iL1=0,iL2=0;
  int v1=0, v2=0;  double refd = 0.0;

  vmapM.resize(Nfp*Nfaces*K);  vmapP.resize(Nfp*Nfaces*K);
  mapM.range(1,Nfp*Nfaces*K);  mapP = mapM;

  int NF = Nfp*Nfaces;

  // number volume nodes consecutively
  IVec nodeids = Range(1,Np*K);

  // find index of face nodes with respect to volume node ordering
  for (k1=1; k1<=K; ++k1) {
    iL1=(k1-1)*NF; iL2=k1*NF;     // define target range in vmapM
    idsL.range(iL1+1, iL2);       // sequential indices for element k1
    idsR = Fmask + (k1-1)*Np;     // offset Fmask for element k1
    vmapM(idsL) = nodeids(idsR);  // map face nodes in element k1
  }

  DVec one(Nfp, 1.0);
  for (k1=1; k1<=K; ++k1) {
    for (f1=1; f1<=Nfaces; ++f1) {

      // find neighbor
      k2 = EToE(k1,f1); f2 = EToF(k1,f1);

      skM = (k1-1)*NF;  // offset to element k1
      skP = (k2-1)*NF;  // offset to element k2

      idsM.range((f1-1)*Nfp+1+skM, f1*Nfp+skM);
      idsP.range((f2-1)*Nfp+1+skP, f2*Nfp+skP);

      // find find volume node numbers of left and right nodes 
      vidM = vmapM(idsM);     vidP = vmapM(idsP);

      x1=x(vidM);  y1=y(vidM);  z1=z(vidM);
      x2=x(vidP);  y2=y(vidP);  z2=z(vidP);

      xM=outer(x1,one);  yM=outer(y1,one);  zM=outer(z1,one);
      xP=outer(x2,one);  yP=outer(y2,one);  zP=outer(z2,one);
      
      // Compute distance matrix
      D = sqr(xM-trans(xP)) + sqr(yM-trans(yP)) + sqr(zM-trans(zP));

      idMP = find2D(abs(D), '<', NODETOL);
      idM=idMP(All,1); idP=idMP(All,2);

      idM += (f1-1)*Nfp + skM;  // offset ids to {f1,k1}
      vmapP(idM) = vidP(idP);   // set external element ids

      idP += (f2-1)*Nfp + skP;  // offset ids to {f2,k2}
      mapP(idM) = idP;          // set external face ids
    }
  }

  // Create list of boundary nodes
  mapB = find(vmapP, '=', vmapM); vmapB = vmapM(mapB);

#if (0)
  dumpIVec(vmapM, "vmapM");
  dumpIVec(vmapP, "vmapP");
  dumpIVec(mapB,   "mapB");
  dumpIVec(vmapB, "vmapB");
  umERROR("Testing", "Nigel, check arrays");
#endif

}
