// BuildPeriodicMaps2D.m
// function BuildPeriodicMaps2D(xperiod, yperiod)
// 2007/06/25
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::BuildPeriodicMaps2D(double xperiod, double yperiod)
//---------------------------------------------------------
{
  // function [] = BuildPeriodicMaps2D(xperiod, yperiod);
  // Purpose: Connectivity and boundary tables for with all
  //          maps returned in Globals2D assuming periodicity

  // Find node to node connectivity
  vmapM.resize(Nfp*Nfaces*K); vmapP.resize(Nfp*Nfaces*K);

  DVec FxL,FyL,FxR,FyR; DMat x1,x2,y1,y2,D,xF1,yF1,xF2,yF2;
  IMat idLR;  IVec idL,idR,vidL,vidR,fidL,fidR; 
  int k1=0,f1=0, k2=0,f2=0; DVec onesNfp=ones(Nfp);
  double dx=0.0, dy=0.0, cx1=0.0,cx2=0.0,cy1=0.0,cy2=0.0;
  double dNfp=(double)Nfp;


  for (k1=1; k1<=K; ++k1) {
    for (f1=1; f1<=Nfaces; ++f1) {

      k2 = EToE(k1,f1); f2 = EToF(k1,f1);
          
      vidL = Fmask(All,f1);  vidL += (k1-1)*Np;
      vidR = Fmask(All,f2);  vidR += (k2-1)*Np;

      fidL = Range(1,Nfp) + (f1-1)*Nfp + (k1-1)*Nfp*Nfaces;
      fidR = Range(1,Nfp) + (f2-1)*Nfp + (k2-1)*Nfp*Nfaces;

      vmapM(fidL) = vidL; vmapP(fidL) = vidL;

      FxL=Fx(fidL); FyL=Fy(fidL); FxR=Fx(fidR); FyR=Fy(fidR);
      x1 = outer(FxL, onesNfp);  y1 = outer(FyL, onesNfp);
      x2 = outer(FxR, onesNfp);  y2 = outer(FyR, onesNfp);

      // Compute distance matrix
      D = sqr(x1-trans(x2)) + sqr(y1-trans(y2));

      idLR = find2D(abs(D), '<', NODETOL);
      idL=idLR(All,1); idR=idLR(All,2);

      vmapP(fidL(idL)) = vidR(idR);
    }
  }



  for (k1=1; k1<=K; ++k1) {
    for (f1=1; f1<=Nfaces; ++f1) {

      //###################################################
      xF1=x(Fmask(All,f1), k1);  cx1=xF1.sum()/dNfp;
      yF1=y(Fmask(All,f1), k1);  cy1=yF1.sum()/dNfp;
      //###################################################

      k2 = EToE(k1,f1); f2 = EToF(k1,f1);    
      if (k2==k1) {
        for (k2=1; k2<=K; ++k2) {
          if (k1!=k2) {
            for (f2=1; f2<=Nfaces; ++f2) {
              if (EToE(k2,f2)==k2) {

                //#########################################
                xF2=x(Fmask(All,f2), k2);  cx2=xF2.sum()/dNfp;
                yF2=y(Fmask(All,f2), k2);  cy2=yF2.sum()/dNfp;
                //#########################################

                dx = sqrt( SQ(abs(cx1-cx2)-xperiod) + SQ(cy1-cy2));
                dy = sqrt( SQ(cx1-cx2) + SQ(abs(cy1-cy2)-yperiod));
                
                if (dx<NODETOL || dy<NODETOL) {
                  EToE(k1,f1) = k2;  EToE(k2,f2) = k1;
                  EToF(k1,f1) = f2;  EToF(k2,f2) = f1;

                  vidL = Fmask(All,f1);  vidL += (k1-1)*Np;
                  vidR = Fmask(All,f2);  vidR += (k2-1)*Np;
                  
                  fidL = Range(1,Nfp) + (f1-1)*Nfp + (k1-1)*Nfp*Nfaces;
                  fidR = Range(1,Nfp) + (f2-1)*Nfp + (k2-1)*Nfp*Nfaces;

                  FxL=Fx(fidL); FyL=Fy(fidL); FxR=Fx(fidR); FyR=Fy(fidR);

                  x1 = outer(FxL, onesNfp);  y1 = outer(FyL, onesNfp);
                  x2 = outer(FxR, onesNfp);  y2 = outer(FyR, onesNfp);
                  
                  // Compute distance matrix
                  if (dx<NODETOL) {
                    D = sqr(abs(x1-trans(x2))-xperiod) + sqr(y1-trans(y2));
                  } else {
                    D = sqr(x1-trans(x2)) + sqr(abs(y1-trans(y2))-yperiod);
                  }

                  idLR = find2D(abs(D), '<', NODETOL);
                  idL=idLR(All,1); idR=idLR(All,2);
                //assert(idL.size() == Nfp);
                  if (idL.size() != Nfp) {
                    umERROR("NDG2D::BuildPeriodicMaps2D", "Nfp != idL.size() = %d", idL.size());
                  }
                  vmapP(fidL(idL)) = vidR(idR);
                  vmapP(fidR(idR)) = vidL(idL);
                }
              }
            }
          }
        }
      }
    }
  }

  // Create default list of boundary nodes
  mapB = find(vmapP, '=', vmapM);  vmapB = vmapM(mapB);
}
