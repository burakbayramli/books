// NGD_pragma.cpp
//
// 2007/06/06
//---------------------------------------------------------
#include "NDG_headers.h"


//---------------------------------------------------------
// Link with CHOLMOD
//---------------------------------------------------------
#ifdef _DEBUG
#pragma message("Linking with CHOLMOD v1.5.0              (Debug Build)")
#pragma comment(lib,"AMDd.lib")
#pragma comment(lib,"CAMDd.lib")
#pragma comment(lib,"CHOLMODd.lib")
#pragma comment(lib,"COLAMDd.lib")
#pragma comment(lib,"Metis4d.lib")
#else
#pragma message("Linking with CHOLMOD v1.5.0              (Release Build)")
#pragma comment(lib,"AMD.lib")
#pragma comment(lib,"CAMD.lib")
#pragma comment(lib,"CHOLMOD.lib")
#pragma comment(lib,"COLAMD.lib")
#pragma comment(lib,"Metis4.lib")
#endif


//---------------------------------------------------------
// Link with Sledge++ modules
//---------------------------------------------------------
#ifdef _DEBUG
#pragma message("Linking with project libraries           (Debug Build)")
//#pragma comment(lib,"NDGLibd.lib")
//#pragma comment(lib,"CSparsed.lib")
//#pragma comment(lib,"UMFPACK5xd.lib")
#else
#pragma message("Linking with project libraries           (Release Build)")
//#pragma comment(lib,"NDGLib.lib")
//#pragma comment(lib,"CSparse.lib")
//#pragma comment(lib,"UMFPACK5x.lib")
#endif


//---------------------------------------------------------
// Link with wxWindows
//---------------------------------------------------------
#ifdef _DEBUG
#pragma message("Linking with project libraries           (Debug Build)")
//#pragma comment(lib,"wxbase26d.lib")
//#pragma comment(lib,"wxmsw26d_core.lib")
//#pragma comment(lib,"wxmsw26d_adv.lib")
//#pragma comment(lib,"wxmsw26d_html.lib")
//#pragma comment(lib,"wxjpegd.lib")
//#pragma comment(lib,"wxtiffd.lib")
//#pragma comment(lib,"wxpngd.lib")
//#pragma comment(lib,"wxregexd.lib")
//#pragma comment(lib,"wxzlibd.lib")
#else
#pragma message("Linking with project libraries           (Release Build)")
//#pragma comment(lib,"wxbase26.lib")
//#pragma comment(lib,"wxmsw26_core.lib")
//#pragma comment(lib,"wxmsw26_adv.lib")
//#pragma comment(lib,"wxmsw26_html.lib")
//#pragma comment(lib,"wxjpeg.lib")
//#pragma comment(lib,"wxtiff.lib")
//#pragma comment(lib,"wxpng.lib")
//#pragma comment(lib,"wxregex.lib")
//#pragma comment(lib,"wxzlib.lib")
#endif



//---------------------------------------------------------
// Link with OpenCascade
//---------------------------------------------------------
//#pragma message("Linking with OpenCascade Libs            (Default Build)")


//---------------------------------------------------------
// Link with GRUMMP 0.3.0
//---------------------------------------------------------
#ifdef _DEBUG
#pragma message("Linking with GRUMMP 0.3.3 libraries      (Debug Build)")
//# pragma comment(lib,"GR_baseD.lib")
//# pragma comment(lib,"GR_2DD.lib")
//# pragma comment(lib,"GR_3DD.lib")
//# pragma comment(lib,"GR_surfD.lib")
//# pragma comment(lib,"OptMSD.lib")
//# pragma comment(lib,"SUMAAlogD.lib")
#else
#pragma message("Linking with GRUMMP 0.3.3 libraries      (Release Build)")
//# pragma comment(lib,"GR_base.lib")
//# pragma comment(lib,"GR_2D.lib")
//# pragma comment(lib,"GR_3D.lib")
//# pragma comment(lib,"GR_surf.lib")
//# pragma comment(lib,"OptMS.lib")
//# pragma comment(lib,"SUMAAlog.lib")
#endif



//---------------------------------------------------------
// link with OpenGL and Vtk libraries
//---------------------------------------------------------
#pragma message("Linking with OpenGL/Vtk libs             (Default Build)")

//#pragma comment(lib,"GLU32.LIB")
//#pragma comment(lib,"OPENGL32.LIB")

//#pragma comment(lib,"vtkCommon.lib")
//#pragma comment(lib,"vtkFiltering.lib")
//#pragma comment(lib,"vtkfreetype.lib")
//#pragma comment(lib,"vtkftgl.lib")
//#pragma comment(lib,"vtkGraphics.lib")
//#pragma comment(lib,"vtkHybrid.lib")
//#pragma comment(lib,"vtkImaging.lib")
//#pragma comment(lib,"vtkIO.lib")
//#pragma comment(lib,"vtkjpeg.lib")
//#pragma comment(lib,"vtkpng.lib")
//#pragma comment(lib,"vtkRendering.lib")
//#pragma comment(lib,"vtktiff.lib")
//#pragma comment(lib,"vtkWidgets.lib")
//#pragma comment(lib,"vtkzlib.lib")


//---------------------------------------------------------
// link with Multimedia libraries
//---------------------------------------------------------
//#pragma comment(lib,"vfw32.lib")


//---------------------------------------------------------
// Link with Intel MKL 5.1 library 
//---------------------------------------------------------
#define USE_DYN_ACML
#undef USE_STATIC_MKL
#undef USE_ATLAS

//---------------------------------------------------------
#if defined(USE_STATIC_MKL)
# pragma message("Linking with Intel MKL 5.1 library       (Static linkage)") 
//# pragma comment(lib,"mkl_c.lib")
//# pragma comment(lib,"mkl_lapack.lib")
//# pragma comment(lib,"mkl_p3.lib")       // faster with PIII
//---------------------------------------------------------
#elif defined(USE_DYN_ACML)
# pragma message("Linking with ACML OpenMP/32 library      (Dynamic linkage)") 
# pragma comment(lib,"libacml_dll.lib")       // single threaded
//# pragma comment(lib,"libacml_mp_dll.lib")      // OpenMP
//---------------------------------------------------------
#elif defined(USE_ATLAS)
# pragma message("Linking with ATLAS Opteron-32/ SSE2      (Dynamic linkage)") 
//# pragma comment(lib,"atlas.lib")
//---------------------------------------------------------
#else
# pragma message("Linking with ARPACK  BLAS/LAPACK         (Dynamic linkage)") 
//# pragma comment(lib,"IVFBLAS.lib")
//# pragma comment(lib,"IVFLAPACK.lib")
//# pragma comment(lib,"BlasLapack.lib")
//# pragma comment(lib,"IVFARPACK.lib")
#endif

//#pragma message("Linking with GOTO Opteron/32 library   (Dynamic linkage)") 
//#pragma comment(lib,"libgoto_opt32-r0.94.lib")


//---------------------------------------------------------
// XDR: ntohl,htonl
//---------------------------------------------------------
//#pragma comment(lib,"Ws2_32.lib")
