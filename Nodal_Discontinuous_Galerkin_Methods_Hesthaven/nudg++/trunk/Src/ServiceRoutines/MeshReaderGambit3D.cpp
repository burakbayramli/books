// MeshReaderGambit3D.cpp
// function [Nv, VX, VY, VZ, K, EToV] = MeshReaderGambit3D(FileName)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include "NDG3D.h"


//---------------------------------------------------------
bool NDG3D::MeshReaderGambit3D(const string& fname)
//---------------------------------------------------------
{
  // function [Nv, VX, VY, K, EToV] = MeshReaderGambit3D(FileName)
  // Purpose  : Read in basic grid information to build grid
  //
  // NOTE     : gambit(Fluent, Inc) *.neu format is assumed

  this->FileName = fname;      // store filename
  if (! file_exists(FileName)) {
    umWARNING("NDG3D::MeshReaderGambit3D()", "cannot find file %s", fname.c_str());
    return false;
  }

  // Nfp=N+1; Np=(N+1)*(N+2)/2; Nfaces=3; 

  ifstream is(FileName.c_str(), std::ios::in);
  if (!is) {
    umWARNING("NDG3D::MeshReaderGambit3D()", "failed to load file %s", fname.c_str());
    return false;
  }

  umTRC(1, "loading geometry from file %s\n", fname.c_str());

  char buf[BUFSIZ]={""};
  bIs3D=false; bCoord3D=false; bElement3D=false;
  int i=0, id=0;

  // Skip 6-line header
  for (i=1; i<=6; ++i)
    is.getline(buf, BUFSIZ);

  //---------------------------------------------
  // Find number of nodes and number of elements
  // [NUMNP NELEM NGRPS NBSETS NDFCD NDFVL]
  //---------------------------------------------
  is >> Nv      // num nodes in mesh
     >> K       // num elements
     >> Nmats   // num material groups
     >> Nbcs    // num boundary groups
     >> Nsd;    // num space dimensions

  is.getline(buf, BUFSIZ);    // clear rest of line
  is.getline(buf, BUFSIZ);    // Skip  "ENDOFSECTION"
  is.getline(buf, BUFSIZ);    // Skip  "NODAL COORDINATES 1.3.0"

  //---------------------------------------------
  // Manage number of space dimensions
  //---------------------------------------------
  if (Nsd<2 || Nsd>3) {
    umWARNING("NDG3D::load_mesh()", "Number of space dimensions was %d?", Nsd);
    return false;
  }

  bIs3D = (3 == Nsd);

  // Allow 3D-coords for 2D elements, e.g. triangles on a sphere
  bool tri_in_3D = false;
  if (false)
    tri_in_3D = true;

  if (bIs3D && !tri_in_3D) {  
    Nfaces = 4;         // Tetrahedra
    bCoord3D = true;
    bElement3D = true;
  } else {
    Nfaces = 3;         // Triangles
    bCoord3D = (tri_in_3D ? true : false);
    bElement3D = false;
  }

  // Triangles or Tetrahedra?
  bool bTET = (bElement3D ? true : false);

  // resize arrays
  VX.resize(Nv);  VY.resize(Nv); VZ.resize(bCoord3D ? Nv : 0);

  // read node coords (order not assumed)
  for (i=1; i<=Nv; ++i) 
  {
    is >> id;   // read id before using it
    is >> VX(id) >> VY(id); 
    if (bCoord3D) { is >> VZ(id); }
    is.getline(buf,BUFSIZ);
  }

  is.getline(buf, BUFSIZ);  // Skip "ENDOFSECTION"
  is.getline(buf, BUFSIZ);  // Skip "ELEMENTS/CELLS 1.3.0"


  //-------------------------------------
  // Triangles in 3D:
  //-------------------------------------
  // ENDOFSECTION
  //    ELEMENTS/CELLS 1.3.0
  //      1  3  3        1       2       3
  //      2  3  3        3       2       4
  //---------------------------------------------
  // Tetrahedra in 3D:
  //---------------------------------------------
  // ENDOFSECTION
  //    ELEMENTS/CELLS 1.3.0
  //     1  6  4      248     247     385     265
  //     2  6  4      248     249     273     397
  //---------------------------------------------

  if (bTET)
    EToV.resize(K, 4); // Tetrahedra
  else
    EToV.resize(K, 3); // Triangles

  int etype=0, nface=0, n1=0, n2=0, n3=0, n4=0;

  // read element to node connectivity
  for (i=1; i<=K; ++i) 
  {
    is >> id >> etype >> nface >> n1 >> n2 >> n3; 
    assert(3==nface || (4==nface && bTET));
    if (4==nface) { is >> n4; }
    is.getline(buf, BUFSIZ);

    EToV(id,1) = n1;
    EToV(id,2) = n2;
    EToV(id,3) = n3;
    if (4==nface) { EToV(id,4) = n4; }
  }

  is.getline(buf, BUFSIZ);  // Skip "ENDOFSECTION"
  is.getline(buf, BUFSIZ);  // Skip "ELEMENT GROUP 1.3.0"


#ifndef NDEBUG
  // Find bounding box for the mesh
  Xmin=VX.min_val();  Xmax=VX.max_val();
  Ymin=VY.min_val();  Ymax=VY.max_val();
  if (bCoord3D) {Zmin=VZ.min_val(); Zmax=VZ.max_val();}

  umTRC(4,   "umMESH : Bounding Box for %d-D mesh\n", Nsd);
  umTRC(4,   "         x (%8.3lf, %8.3lf)\n", Xmin,Xmax);
  umTRC(4,   "         y (%8.3lf, %8.3lf)\n", Ymin,Ymax);
  if (bCoord3D) {
    umTRC(4, "         z (%8.3lf, %8.3lf)\n", Zmin,Zmax);}
#endif


  /////////////////////////////////////////////////////////
  //
  // if present, load material "epsilon" for each element
  //
  /////////////////////////////////////////////////////////

  //-------------------------------------------------------
  // ENDOFSECTION
  //        ELEMENT GROUP 1.3.0
  // GROUP:           1 ELEMENTS:        512 MATERIAL:      1.000 NFLAGS:          0 
  //                   epsilon: 1.000
  //        0
  //        1       2       3       4       5       6       7       8       9      10
  //       11      12      13      14      15      16      17      18      19      20
  //       21      22      23      24      25      26      27      28      29      30
  //------------------------------------------------------

  double gepsln = 6.5;      // [0:13] Map colors to BC type
//double gepsln = 3.0;      // [0:6]  Map colors to AMR cell status

  // Allocate a scalar for each element
  epsilon.resize(K, true, gepsln);

  //---------------------------------------------
  // Each element belongs to a "material group"
  // with a scalar "epsilon" value.  Associate 
  // this scalar with each element in the group:
  // AMR: 
  // Adaptive meshes track element material!
  //---------------------------------------------
  materialVals.resize(Nmats);

  std::string gname;
  int nread=0, gnum=0, gnel=0;

  for (i=1; i<=Nmats; ++i)
  {
    //-----------------------------------
    // Read group ID, nel, epsilon
    //-----------------------------------
    is.getline(buf, BUFSIZ);
    // > GROUP:  1  ELEMENTS:  698  MATERIAL:  12.000 
    nread = sscanf(buf,"%*s%d%*s%d%*s%lf", &gnum,&gnel,&gepsln);
    assert(3==nread);

    if (gepsln <= 0.0) {
      //gepsln = 1.0;
      umMSG(1, "umMESH :  epsilon for material %d was <= 0.0\n", i);
    } else if (gepsln < 1.0) {
      umMSG(1, "umMESH :  epsilon for material %d was < 1.0\n", i);
    }

    // TODO: Map multiple materials under transparent fields
    materialVals(i) = gepsln;
  //materialVals(i) = i;

    umTRC(3, "umMESH : Region %2d has %6d elements, with epsilon = %7.3lf\n", gnum, gnel, gepsln);

    // Read group name
    is.getline(buf, BUFSIZ);
    gname = trim(buf);

    // Skip solver-dependent flags
    is.getline(buf, BUFSIZ);

    // Load epsilon for elements in group
    for (int k=1; k<=gnel; ++k) {
      is >> id;
      epsilon(id) = gepsln;
    }

    is.getline(buf, BUFSIZ);  // Clear end of line
    is.getline(buf, BUFSIZ);  // Skip "ENDOFSECTION"
    if (! strstr(buf, "ENDOFSECTION")) {
      umWARNING("NDG3D::load_mesh()", "Expected [ENDOFSECTION]");
      return false;
    }
    is.getline(buf, BUFSIZ);  // Skip "ELEMENT GROUP 1.3.0"
  }


  /////////////////////////////////////////////////////////
  //
  // Boundary groups
  //
  /////////////////////////////////////////////////////////

  // boundary codes (defined in Globals2D)
  if (bTET) {
    BCType.resize(K, 4); // Tetrahedra
  } else {
    BCType.resize(K, 3); // Triangles
  }

  for (i=1; i<=Nbcs; ++i)
  {
    is.getline(buf, BUFSIZ);  // Load ith boundary group
    if (strstr(buf, "ENDOFSECTION")) {
      continue;       // demos may have no boundary info
    }

    if (! load_BF_group(is, buf)) {
      umWARNING("NDG3D::load_mesh()", "Error loading boundary face group: %s\n", buf);
      return false;
    }

    is.getline(buf, BUFSIZ);  // Clear "end of line"
    is.getline(buf, BUFSIZ);  // Skip "ENDOFSECTION"
    if (! strstr(buf, "ENDOFSECTION")) {
      umWARNING("NDG3D::load_mesh()", "Expected [ENDOFSECTION]");
      return false;
    }
    is.getline(buf, BUFSIZ);  // Skip "ELEMENT GROUP 1.3.0"
  }

  is.close();     // Finished reading from the file stream.
  return true;    // mesh data loaded successfully
}


//---------------------------------------------------------
bool NDG3D::load_BF_group(istream& is, char* buf)
//---------------------------------------------------------
{
  int bcNF=0, bcCNT=0, bcflag = BC_None;
  int nread=0, elmt=0, dum=0, face=0, bcID=0;
  double radius=0.0; char bcTXT[100]; std::string name;

  //---------------------------------------------
  // Examine group header (1 line held in buf)
  //---------------------------------------------
  // >      Far       1      64       0       0
  //---------------------------------------------
  if (strstr(buf, "Cyl")) {
          nread = sscanf(buf,"%s %lf %d", bcTXT, &radius, &bcNF);
  } else {nread = sscanf(buf,"%s %d %d",  bcTXT, &bcID,   &bcNF);}
  assert(3==nread);

  // Extract name of boundary condition 
  name = trim(bcTXT);

  umTRC(3, "Loading %4d faces with BC %s\n", bcNF, name.c_str()); 
  if      (match_BC(name, "Infl"))  bcflag = BC_In;
  else if (match_BC(name, "Outf"))  bcflag = BC_Out;
  else if (match_BC(name, "Wall"))  bcflag = BC_Wall;
  else if (match_BC(name, "Far" ))  bcflag = BC_Far;
  else if (match_BC(name, "Cyl" ))  bcflag = BC_Cyl;
  else if (match_BC(name, "Diri"))  bcflag = BC_Dirichlet;
  else if (match_BC(name, "Neum"))  bcflag = BC_Neuman;
  else if (match_BC(name, "Slip"))  bcflag = BC_Slip;
  else { 
    umWARNING("NDG3D::load_BF_group()", "Unexpected boundary condition group, '%s' ", name.c_str());
    return false;
  }

  bcCNT = 0;
  for (int bf=1; bf<=bcNF; ++bf) {
    is >> elmt >> dum >> face;      // read data
    BCType(elmt, face) = bcflag;    // mark face with BC
    ++bcCNT;                        // adjust counter
    if (! is.good()) break;         // check stream
    assert((elmt<=K) && (face<=4)); // check values (tri/tet)
  }

  return (bcCNT==bcNF) ? true : false;
}
