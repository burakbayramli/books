// TopTheta.cpp
// function topu = TopTheta(u, theta)
// 2007/10/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


// template <class T> void Sort_Index(const Vector<T>& A, Vector<T>& S, IVec& IX, eDir dir); // =eAscend
void Sort_Index(const DVec& A, DVec& S, IVec& IX, eDir dir); // =eAscend


//---------------------------------------------------------
IVec& TopTheta(const DVec& u, double theta, bool do_print)
//---------------------------------------------------------
{
  DVec sortu("sortu"); IVec indu("indu");

  IVec* tmp = new IVec("topu", OBJ_temp); IVec& topu=(*tmp);

  // [sortu,indu] = sort(u(:), eDescend);
//sort              (u, sortu, indu, eDescend);
//Sort_Index<double>(u, sortu, indu, eDescend);
  Sort_Index        (u, sortu, indu, eDescend);


  //#######################################################
  // FIXME: ask Tim if this should be u.sum_abs();
  // NBN: check for numerical noise:
  double u_sum = u.sum();
  if (fabs(u_sum)<5e-14)
//if (0.0 == u_sum)
  {
    return topu;        // return empty vector
  }
  //#######################################################

  DVec cumsortu = cumsum(sortu) / u_sum;

  topu = find(cumsortu, '<', theta);

  if (topu.size()<1) {
    if (cumsortu(1)>1e-10) {
      // topu.resize(1); topu(1)=indu(1);
      topu.append(indu(1));
    }
  } else {
    topu = indu.get_map(topu);
  }

  if (do_print) 
  {
  //static int cntx=0;  char buf[20]={""};
  //sprintf(buf, "topu_%d", ++cntx);

  //dumpDVec(u,        "u");
  //dumpDVec(sortu,    "sortu");
  //dumpIVec(indu,     "indu");
  //dumpDVec(cumsortu, "cumsortu");
  //dumpIVec(topu,      buf);
  }


  return topu;
}
