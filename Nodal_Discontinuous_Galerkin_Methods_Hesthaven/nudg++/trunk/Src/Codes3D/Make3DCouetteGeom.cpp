// Make3DCouetteGeom.cpp
// 
// 2007/07/10
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void Make3DCouetteGeom
( 
  double r1, int Ntheta1,   // inner cylinder
  double r2, int Ntheta2,   // outer cylinder
  double zh, int Nz         // height and vert. slices
)
//---------------------------------------------------------
{
  if (Ntheta1<3 || Ntheta1>1024) { umWARNING("Make3DCouetteGeom", "Please set Ntheta1 in {3:1024}.  Was %d", Ntheta1); return; }
  if (Ntheta2<3 || Ntheta2>1024) { umWARNING("Make3DCouetteGeom", "Please set Ntheta2 in {3:1024}.  Was %d", Ntheta2); return; }

  // allocate storage
  DMat c1Top(3,Ntheta1), c1Bse(3,Ntheta1);
  DMat c2Top(3,Ntheta2), c2Bse(3,Ntheta2);
  double x=0.0; int i=0,j=0,Nv=0,face=0,sk=0;

  //-------------------------------------
  // Build inner circle
  //-------------------------------------
  
  // set angle increment (resolution) for inner circle
  double dtheta1 = TWOPI/double(Ntheta1);
  for (i=1; i<=Ntheta1; ++i) {
    x=(i-1)*dtheta1;
    c1Top(1,i)=r1*cos(x); c1Top(2,i)=r1*sin(x);c1Top(3,i)=zh;
    c1Bse(1,i)=r1*cos(x); c1Bse(2,i)=r1*sin(x);c1Bse(3,i)=0.0;
  }

  //-------------------------------------
  // Build outer circle
  //-------------------------------------

  // set angle increment (resolution) for outer circle
  double dtheta2 = TWOPI/double(Ntheta2);
  for (i=1; i<=Ntheta2; ++i) {
    x=(i-1)*dtheta2; 
    c2Top(1,i)=r2*cos(x); c2Top(2,i)=r2*sin(x); c2Top(3,i)=zh;
    c2Bse(1,i)=r2*cos(x); c2Bse(2,i)=r2*sin(x); c2Bse(3,i)=0.0;
  }

  //dumpDMat(trans(c1Top), "c1Top");
  //dumpDMat(trans(c2Top), "c2Top");

  //-------------------------------------
  // Write GRUMMP .bdry format
  //-------------------------------------
  int Nvert = (2*Ntheta2) + (2*Ntheta1);
  int Npoly = Ntheta2 + Ntheta1;

  string fname("Couette.bdry");
  FILE* fp=fopen(fname.c_str(), "w");
  fprintf(fp, "#\n# Boundary geometry defining concentric cylinders \n");
  fprintf(fp, "# for cylindrical Taylor-Couette flow simulation:\n#\n");
  fprintf(fp, "# %d vertices %d boundary polygons\n#\n", Nvert, Npoly+4);
  fprintf(fp, "%d  %d\n", Nvert, Npoly+4);
  fprintf(fp, "#\n# Vertex coordinates\n#\n");

  // write top of outer cylinder
  for (i=1; i<=Ntheta2; ++i) { fprintf(fp, "%16.12lf  %16.12lf  %16.12lf\n", c2Top(1,i), c2Top(2,i), c2Top(3,i)); }
  // write base of outer cylinder
  for (i=1; i<=Ntheta2; ++i) { fprintf(fp, "%16.12lf  %16.12lf  %16.12lf\n", c2Bse(1,i), c2Bse(2,i), c2Bse(3,i)); }

  // write top of inner cylinder
  for (i=1; i<=Ntheta1; ++i) { fprintf(fp, "%16.12lf  %16.12lf  %16.12lf\n", c1Top(1,i), c1Top(2,i), c1Top(3,i)); }
  // write base of inner cylinder
  for (i=1; i<=Ntheta1; ++i) { fprintf(fp, "%16.12lf  %16.12lf  %16.12lf\n", c1Bse(1,i), c1Bse(2,i), c1Bse(3,i)); }

  //-------------------------------------
  // Set ids of the 4 verts in each face
  //-------------------------------------

  // 1: outer cylinder wall
  IVec id(Ntheta2*(4+1) + Ntheta1*(4+1));
  sk=1;
  for (face=0; face<Ntheta2; ++face) {
    id(sk++) = 4;       // num verts in poly
    id(sk++) = 1+  face;
    id(sk++) = 1+ (face+1)%(Ntheta2);
    id(sk++) = 1+ (face+1)%(Ntheta2) + Ntheta2;
    id(sk++) = 1+ (face  )%(Ntheta2) + Ntheta2;
  }

  // 2: inner cylinder wall
  int offset = (2*Ntheta2)+1;
  for (face=0; face<Ntheta1; ++face) {
    id(sk++) = 4;       // num verts in poly
    id(sk++) = offset+  face;
    id(sk++) = offset+ (face  )%(Ntheta1) + Ntheta1;
    id(sk++) = offset+ (face+1)%(Ntheta1) + Ntheta1;
    id(sk++) = offset+ (face+1)%(Ntheta1);
  }

  int NvInPatch = 2+(Ntheta2+Ntheta1)/2;

  // Write the top 2 polygons (make annulus)
  IVec idTop1 (NvInPatch), idTop2 (NvInPatch);
  IVec idBase1(NvInPatch), idBase2(NvInPatch);

  int t2A = 1, t2B = (1+Ntheta2/2), t2C = Ntheta2;
  int t1A = 1, t1B = (1+Ntheta1/2), t1C = Ntheta1;

  int offset1 = (2*Ntheta2) + Ntheta1/2;
  int offset2 =  2*Ntheta2;

  //-------------------------------------
  // set ids for 1st half of top annulus
  //-------------------------------------
  idTop1(1)=1;  sk=2;
  for (i=2; i<=t2B; ++i) {  idTop1(sk++) = (Ntheta2-i)+2;  }
  for (i=1; i< t1B; ++i) {  idTop1(sk++) = offset1 + i;  }
  idTop1(sk)=offset2+1;

  // set ids for 2nd half of top annulus
  sk=1;
  for (i=1; i<=t2B; ++i) {  idTop2(sk++) = (t2B-i)+1;  }
  for (i=1; i<=t1B; ++i) {  idTop2(sk++) = offset2 + i;  }

  offset1 = (2*Ntheta2) + Ntheta1 + Ntheta1/2 + 2;
  offset2 =  Ntheta2 + Ntheta2/2;
  int offset3 = Nvert + 2;

  //-------------------------------------
  // set ids for 1st half of base annulus
  //-------------------------------------
  sk=1;
  for (i=1; i<=t2B; ++i) {  idBase1(sk++) = Ntheta2+i;  }
  for (i=1; i<=t1B; ++i) {  idBase1(sk++) = offset1 - i;  }

  //-------------------------------------
  // set ids for 2nd half of base annulus
  //-------------------------------------
  sk=1;
  for (i=1; i< t2B; ++i) {  idBase2(sk++) = offset2 + i;  }
  idBase2(sk++) = Ntheta2+1;
  idBase2(sk++) = 2*Ntheta2 + Ntheta1 + 1;
  for (i=2; i<=t1B; ++i) {  idBase2(sk++) = offset3-i;  }


  fprintf(fp, "#\n# boundary entities\n#\n");

  //-------------------------------------
  // write the Npoly vertical faces
  //-------------------------------------
  sk=1;
  for (i=1; i<=Ntheta2; ++i) {
    Nv = id(sk++);
    fprintf(fp, "polygon b 1 r 1 %d  ", Nv);
    for (j=1; j<=Nv; ++j) {
      // write (zero-based) index of vertex j
      fprintf(fp, " %d", id(sk++)-1);
    //fprintf(fp, " %d", id(sk++));
    }
    fprintf(fp, "\n");
  }
  for (i=1; i<=Ntheta1; ++i) {
    Nv = id(sk++);
    fprintf(fp, "polygon b 2 r 1 %d  ", Nv);
    for (j=1; j<=Nv; ++j) {
      // write (zero-based) index of vertex j
      fprintf(fp, " %d", id(sk++)-1);
    //fprintf(fp, " %d", id(sk++));
    }
    fprintf(fp, "\n");
  }


  //-------------------------------------
  // write the top of the cylinder          (zero-based indices)
  //-------------------------------------
  fprintf(fp, "polygon b 3 r 1 %d  ", NvInPatch);
  for (i=1; i<=NvInPatch; ++i) { fprintf(fp, " %d", idTop1(i)-1); } fprintf(fp, "\n");
  fprintf(fp, "polygon b 3 r 1 %d  ", NvInPatch);
  for (i=1; i<=NvInPatch; ++i) { fprintf(fp, " %d", idTop2(i)-1);} fprintf(fp, "\n");


  //-------------------------------------
  // write the base of the cylinder
  //-------------------------------------
  fprintf(fp, "polygon b 3 r 1 %d  ", NvInPatch);
  for (i=1; i<=NvInPatch; ++i) { fprintf(fp, " %d", idBase1(i)-1); } fprintf(fp, "\n");
  fprintf(fp, "polygon b 3 r 1 %d  ", NvInPatch);
  for (i=1; i<=NvInPatch; ++i) { fprintf(fp, " %d", idBase2(i)-1);} fprintf(fp, "\n");

  umLOG(1, "\nCouette Bdry geometry wrtten to file %s.\n\n", fname.c_str());
}
