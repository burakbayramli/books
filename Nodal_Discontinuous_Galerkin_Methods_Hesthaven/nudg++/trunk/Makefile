include ./system.mk

INCLUDES = -I./Include

CXXFLAGS = $(CXXOPTIONS) $(OPTFLAGS) $(INCLUDES)
FCFLAGS = $(FCOPTIONS) $(OPTFLAGS)

.SUFFIXES: .cpp .f


OBJS = \
  Src/Arrays/ArrayMacros.o  \
  Src/Arrays/Mat_COL.o       \
  Src/Arrays/Sort_Index.o     \
  Src/Codes1D/GradJacobiP.o    \
  Src/Codes1D/JacobiGL.o        \
  Src/Codes1D/JacobiGQ.o         \
  Src/Codes1D/JacobiP.o           \
  Src/Codes1D/Vandermonde1D.o      \
  Src/Codes2D/BuildBCMaps2D.o       \
  Src/Codes2D/BuildCurvedOPS2D.o     \
  Src/Codes2D/BuildMaps2D.o           \
  Src/Codes2D/BuildPeriodicMaps2D.o   \
  Src/Codes2D/ConformingHrefine2D.o   \
  Src/Codes2D/Connect2D.o             \
  Src/Codes2D/Cub2D.o                 \
  Src/Codes2D/Cubature2D.o            \
  Src/Codes2D/CubatureData2D.o        \
  Src/Codes2D/CubatureVolumeMesh2D.o  \
  Src/Codes2D/Curl2D.o                \
  Src/Codes2D/CurvedDGDiv2D.o         \
  Src/Codes2D/CurvedDGGrad2D.o        \
  Src/Codes2D/CurvedDGJump2D.o        \
  Src/Codes2D/CurvedPoissonIPDG2D.o   \
  Src/Codes2D/CurvedPoissonIPDGbc2D.o \
  Src/Codes2D/CutOffFilter2D.o        \
  Src/Codes2D/Div2D.o               \
  Src/Codes2D/Dmatrices2D.o         \
  Src/Codes2D/dtscale2D.o           \
  Src/Codes2D/Filter2D.o            \
  Src/Codes2D/FindLocalCoords2D.o   \
  Src/Codes2D/GaussFaceMesh2D.o     \
  Src/Codes2D/GeometricFactors2D.o  \
  Src/Codes2D/Globals2D.o           \
  Src/Codes2D/Grad2D.o              \
  Src/Codes2D/GradSimplex2DP.o      \
  Src/Codes2D/GradVandermonde2D.o   \
  Src/Codes2D/Hrefine2D.o           \
  Src/Codes2D/InterpMatrix2D.o      \
  Src/Codes2D/Lift2D.o              \
  Src/Codes2D/MakeCylinder2D.o      \
  Src/Codes2D/NDG2D.o               \
  Src/Codes2D/NDG2D_Output.o        \
  Src/Codes2D/NDG2DDriver.o         \
  Src/Codes2D/Nodes2D.o             \
  Src/Codes2D/Normals2D.o           \
  Src/Codes2D/PhysDmatrices2D.o     \
  Src/Codes2D/rstoab.o              \
  Src/Codes2D/Sample2D.o            \
  Src/Codes2D/Simplex2DP.o          \
  Src/Codes2D/StartUp2D.o           \
  Src/Codes2D/tiConnect2D.o         \
  Src/Codes2D/Vandermonde2D.o       \
  Src/Codes2D/Warpfactor.o          \
  Src/Codes2D/xytors.o              \
  Src/Codes3D/BuildBCMaps3D.o       \
  Src/Codes3D/BuildMaps3D.o         \
  Src/Codes3D/Cub3D.o                \
  Src/Codes3D/Cubature3D.o           \
  Src/Codes3D/CubatureVolumeMesh3D.o \
  Src/Codes3D/Curl3D.o               \
  Src/Codes3D/Div3D.o                \
  Src/Codes3D/Dmatrices3D.o         \
  Src/Codes3D/dtscale3D.o           \
  Src/Codes3D/EquiNodes3D.o         \
  Src/Codes3D/evalshift.o           \
  Src/Codes3D/evalwarp.o            \
  Src/Codes3D/FaceData3D.o          \
  Src/Codes3D/FindLocalCoords3D.o   \
  Src/Codes3D/GeometricFactors3D.o  \
  Src/Codes3D/Globals3D.o           \
  Src/Codes3D/Grad3D.o              \
  Src/Codes3D/GradSimplex3DP.o      \
  Src/Codes3D/GradVandermonde3D.o   \
  Src/Codes3D/Hrefine3D.o           \
  Src/Codes3D/InterpMatrix3D.o      \
  Src/Codes3D/InterpNodeShapes3D.o  \
  Src/Codes3D/IntersectTest3D.o     \
  Src/Codes3D/Lift3D.o              \
  Src/Codes3D/Make3DCouetteGeom.o   \
  Src/Codes3D/NDG3D.o               \
  Src/Codes3D/NDG3DDriver.o         \
  Src/Codes3D/NDG3D_Output.o        \
  Src/Codes3D/Nodes3D.o             \
  Src/Codes3D/Normals3D.o           \
  Src/Codes3D/PartialLiftData3D.o   \
  Src/Codes3D/PhysDmatrices3D.o     \
  Src/Codes3D/PoissonIPDG3D.o       \
  Src/Codes3D/PoissonIPDGbc3D.o     \
  Src/Codes3D/Poly3D.o              \
  Src/Codes3D/rsttoabc.o            \
  Src/Codes3D/Sample3D.o            \
  Src/Codes3D/Simplex3DP.o          \
  Src/Codes3D/StartUp3D.o            \
  Src/Codes3D/tiConnect3D.o           \
  Src/Codes3D/TopTheta.o               \
  Src/Codes3D/Vandermonde3D.o           \
  Src/Codes3D/WarpShiftFace3D.o          \
  Src/Codes3D/xyztorst.o                  \
  Src/ServiceRoutines/Global_funcs.o       \
  Src/ServiceRoutines/INIT.o               \
  Src/ServiceRoutines/LOG.o                \
  Src/ServiceRoutines/MeshReaderGambit2D.o \
  Src/ServiceRoutines/MeshReaderGambit3D.o \
  Src/ServiceRoutines/Tokenizer.o          \
  Src/Sparse/CHOLMOD_solver.o              \
  Src/Sparse/CS_Cholinc.o                  \
  Src/Sparse/CS_Solve.o                    \
  Src/Sparse/CS_Utils.o 

EULOBJS = \
  Src/Examples2D/CurvedEuler2D/ChannelBC2D.o           \
  Src/Examples2D/CurvedEuler2D/ChannelIC2D.o           \
  Src/Examples2D/CurvedEuler2D/CouetteBC2D.o           \
  Src/Examples2D/CurvedEuler2D/CouetteIC2D.o           \
  Src/Examples2D/CurvedEuler2D/CurvedEuler2D.o         \
  Src/Examples2D/CurvedEuler2D/CurvedEuler2D_Driver.o  \
  Src/Examples2D/CurvedEuler2D/CurvedEuler2D_Fluxes.o  \
  Src/Examples2D/CurvedEuler2D/CurvedEuler2D_RHS.o     \
  Src/Examples2D/CurvedEuler2D/CurvedEuler2D_Run.o     \
  Src/Examples2D/CurvedEuler2D/EulerHLL2D.o            \
  Src/Examples2D/CurvedEuler2D/EulerLF2D.o             \
  Src/Examples2D/CurvedEuler2D/EulerRoe2D.o            \
  Src/Examples2D/CurvedEuler2D/IsentropicVortexBC2D.o  \
  Src/Examples2D/CurvedEuler2D/IsentropicVortexIC2D.o

EULOBJS3D = \
  Src/Examples3D/Euler3D/Euler3D.o              \
  Src/Examples3D/Euler3D/Euler3D_Driver.o       \
  Src/Examples3D/Euler3D/Euler3D_Fluxes.o       \
  Src/Examples3D/Euler3D/Euler3D_RHS.o          \
  Src/Examples3D/Euler3D/Euler3D_Run.o          \
  Src/Examples3D/CouetteBC3D.o                  \
  Src/Examples3D/CouetteIC3D.o                  \
  Src/Examples3D/Euler3D/IsentropicVortexBC3D.o \
  Src/Examples3D/Euler3D/IsentropicVortexIC3D.o

CNSOBJS = \
  Src/Examples2D/CurvedCNS2D/BoxFlowBC2D.o        \
  Src/Examples2D/CurvedCNS2D/BoxFlowIC2D.o        \
  Src/Examples2D/CurvedCNS2D/ChannelBC2D.o        \
  Src/Examples2D/CurvedCNS2D/ChannelIC2D.o        \
  Src/Examples2D/CurvedCNS2D/CurvedCNS2D.o        \
  Src/Examples2D/CurvedCNS2D/CurvedCNS2D_Driver.o \
  Src/Examples2D/CurvedCNS2D/CurvedCNS2D_RHS.o    \
  Src/Examples2D/CurvedCNS2D/CurvedCNS2D_Run.o    \
  Src/Examples2D/CurvedCNS2D/CylBC2D.o            \
  Src/Examples2D/CurvedCNS2D/CylIC2D.o  

INSOBJS = \
  Src/Examples2D/CurvedINS2D/CurvedINS2D.o              \
  Src/Examples2D/CurvedINS2D/CurvedINS2D_Driver.o       \
  Src/Examples2D/CurvedINS2D/CurvedINS2D_Run.o          \
  Src/Examples2D/CurvedINS2D/CurvedINSPressureSetUp2D.o \
  Src/Examples2D/CurvedINS2D/CurvedINSViscous2D.o       \
  Src/Examples2D/CurvedINS2D/CurvedINSViscousSetUp2D.o  \
  Src/Examples2D/CurvedINS2D/INSAdvection2D.o           \
  Src/Examples2D/CurvedINS2D/INSLiftDrag2D.o            \
  Src/Examples2D/CurvedINS2D/INSPressure2D.o            \
                                                        \
  Src/Examples2D/CurvedINS2D/INScylinderBC2D.o          \
  Src/Examples2D/CurvedINS2D/INScylinderIC2D.o          \
  Src/Examples2D/CurvedINS2D/KovasznayBC2D.o            \
  Src/Examples2D/CurvedINS2D/KovasznayIC2D.o            \
  Src/Examples2D/CurvedINS2D/PearsonVortexBC2D.o        \
  Src/Examples2D/CurvedINS2D/PearsonVortexIC2D.o        \
  Src/Examples2D/CurvedINS2D/INS_IC_BC_TODO.o


MAXOBJS = \
  Src/Examples2D/Maxwell2D/Maxwell2D.o           \
  Src/Examples2D/Maxwell2D/Maxwell2D_Driver.o    \
  Src/Examples2D/Maxwell2D/Maxwell2D_RHS.o       \
  Src/Examples2D/Maxwell2D/Maxwell2D_Run.o       \
                                                            \
  Src/Examples2D/MaxwellCurved2D/MaxwellCurved2D.o          \
  Src/Examples2D/MaxwellCurved2D/MaxwellCurved2D_Driver.o   \
  Src/Examples2D/MaxwellCurved2D/MaxwellCurved2D_RHS.o      \
                                                            \
  Src/Examples2D/MaxwellNonCon2D/BuildHNonCon2D.o         \
  Src/Examples2D/MaxwellNonCon2D/BuildPNonCon2D.o         \
  Src/Examples2D/MaxwellNonCon2D/MaxwellNonCon2D.o        \
  Src/Examples2D/MaxwellNonCon2D/MaxwellNonCon2D_Driver.o \
  Src/Examples2D/MaxwellNonCon2D/MaxwellNonCon2D_RHS.o    \
  Src/Examples2D/MaxwellNonCon2D/MaxwellNonCon2D_Run.o    \
  Src/Examples2D/MaxwellNonCon2D/MaxwellNonCon2D_Test_H.o \
  Src/Examples2D/MaxwellNonCon2D/MaxwellNonCon2D_Test_P.o \
                                                \
  Src/Examples3D/Maxwell3D/Maxwell3D.o          \
  Src/Examples3D/Maxwell3D/Maxwell3D_Driver.o   \
  Src/Examples3D/Maxwell3D/Maxwell3D_RHS.o      \
  Src/Examples3D/Maxwell3D/Maxwell3D_Run.o      


ARBCOBJS = \
  Src/Examples2D/MaxwellARBC/ARBCplane2D.o         \
  Src/Examples2D/MaxwellARBC/ARBCplane2D_Driver.o  \
  Src/Examples2D/MaxwellARBC/ARBCplane2D_Ops.o     \
  Src/Examples2D/MaxwellARBC/ARBCplane2D_RHS.o     \
  Src/Examples2D/MaxwellARBC/ARBCplane2D_Run.o     \
  Src/Examples2D/MaxwellARBC/DG_Ops_1D.o           \
  Src/Examples2D/MaxwellARBC/wts500_f.o	\
  Src/Examples2D/MaxwellARBC/PECCylSolution2D.o

SHOCKOBJS = \
  Src/Examples2D/EulerShock2D/EulerLimiter2D.o        \
  Src/Examples2D/EulerShock2D/EulerShock2D.o          \
  Src/Examples2D/EulerShock2D/EulerShock2D_Driver.o   \
  Src/Examples2D/EulerShock2D/EulerShock2D_Run.o      \
  Src/Examples2D/EulerShock2D/EulerShock2D_Run_Iter.o \
  Src/Examples2D/EulerShock2D/ForwardStepBC2D.o       \
  Src/Examples2D/EulerShock2D/ForwardStepIC2D.o       \
  Src/Examples2D/EulerShock2D/InletBC2D.o             \
  Src/Examples2D/EulerShock2D/InletIC2D.o 

T3DOBJS = \
  Src/Examples3D/TestPoissonIPDG3D/TestPoissonIPDG3D.o                \
  Src/Examples3D/NonCon3D/FindNonCon3D_V2.o         \
  Src/Examples3D/NonCon3D/IPDGErrorNorm3D.o         \
  Src/Examples3D/NonCon3D/NonConPoissonIPDG3D_V2.o  \
  Src/Examples3D/NonCon3D/ROHOP3D.o                 \
  Src/Examples3D/NonCon3D/ROHOP3D_BdryTerms.o       \
  Src/Examples3D/NonCon3D/ROHOP3D_Driver.o          \
  Src/Examples3D/NonCon3D/ROHOP3D_Funcs.o           \
  Src/Examples3D/NonCon3D/ROHOP3D_Run.o


BLASOBJS = \
 BlasLapack/dasum.o \
 BlasLapack/daxpy.o \
 BlasLapack/dbdsqr.o \
 BlasLapack/dcabs1.o \
 BlasLapack/dcopy.o \
 BlasLapack/ddot.o \
 BlasLapack/dgebak.o \
 BlasLapack/dgebal.o \
 BlasLapack/dgebd2.o \
 BlasLapack/dgebrd.o \
 BlasLapack/dgecon.o \
 BlasLapack/dgeev.o \
 BlasLapack/dgehd2.o \
 BlasLapack/dgehrd.o \
 BlasLapack/dgelq2.o \
 BlasLapack/dgelqf.o \
 BlasLapack/dgelss.o \
 BlasLapack/dgemm.o \
 BlasLapack/dgemv.o \
 BlasLapack/dgeqr2.o \
 BlasLapack/dgeqrf.o \
 BlasLapack/dger.o \
 BlasLapack/dgesv.o \
 BlasLapack/dgesvd.o \
 BlasLapack/dgetf2.o \
 BlasLapack/dgetrf.o \
 BlasLapack/dgetrs.o \
 BlasLapack/dhseqr.o \
 BlasLapack/dlabad.o \
 BlasLapack/dlabrd.o \
 BlasLapack/dlacon.o \
 BlasLapack/dlacpy.o \
 BlasLapack/dladiv.o \
 BlasLapack/dlae2.o \
 BlasLapack/dlaev2.o \
 BlasLapack/dlahqr.o \
 BlasLapack/dlahrd.o \
 BlasLapack/dlaln2.o \
 BlasLapack/dlamch.o \
 BlasLapack/dlange.o \
 BlasLapack/dlanhs.o \
 BlasLapack/dlanst.o \
 BlasLapack/dlansy.o \
 BlasLapack/dlanv2.o \
 BlasLapack/dlapy2.o \
 BlasLapack/dlarf.o \
 BlasLapack/dlarfb.o \
 BlasLapack/dlarfg.o \
 BlasLapack/dlarft.o \
 BlasLapack/dlarfx.o \
 BlasLapack/dlartg.o \
 BlasLapack/dlas2.o \
 BlasLapack/dlascl.o \
 BlasLapack/dlaset.o \
 BlasLapack/dlasq1.o \
 BlasLapack/dlasq2.o \
 BlasLapack/dlasq3.o \
 BlasLapack/dlasq4.o \
 BlasLapack/dlasq5.o \
 BlasLapack/dlasq6.o \
 BlasLapack/dlasr.o \
 BlasLapack/dlasrt.o \
 BlasLapack/dlassq.o \
 BlasLapack/dlasv2.o \
 BlasLapack/dlaswp.o \
 BlasLapack/dlatrd.o \
 BlasLapack/dlatrs.o \
 BlasLapack/dnrm2.o \
 BlasLapack/dorg2l.o \
 BlasLapack/dorg2r.o \
 BlasLapack/dorgbr.o \
 BlasLapack/dorghr.o \
 BlasLapack/dorgl2.o \
 BlasLapack/dorglq.o \
 BlasLapack/dorgql.o \
 BlasLapack/dorgqr.o \
 BlasLapack/dorgtr.o \
 BlasLapack/dorm2r.o \
 BlasLapack/dormbr.o \
 BlasLapack/dorml2.o \
 BlasLapack/dormlq.o \
 BlasLapack/dormqr.o \
 BlasLapack/dpbtf2.o \
 BlasLapack/dpbtrf.o \
 BlasLapack/dpbtrs.o \
 BlasLapack/dposv.o \
 BlasLapack/dpotf2.o \
 BlasLapack/dpotrf.o \
 BlasLapack/dpotrs.o \
 BlasLapack/drot.o \
 BlasLapack/drscl.o \
 BlasLapack/dscal.o \
 BlasLapack/dsteqr.o \
 BlasLapack/dsterf.o \
 BlasLapack/dstev.o \
 BlasLapack/dswap.o \
 BlasLapack/dsyev.o \
 BlasLapack/dsymv.o \
 BlasLapack/dsyr.o \
 BlasLapack/dsyr2.o \
 BlasLapack/dsyr2k.o \
 BlasLapack/dsyrfs.o \
 BlasLapack/dsyrk.o \
 BlasLapack/dsytd2.o \
 BlasLapack/dsytrd.o \
 BlasLapack/dtbsv.o \
 BlasLapack/dtrevc.o \
 BlasLapack/dtrmm.o \
 BlasLapack/dtrmv.o \
 BlasLapack/dtrsm.o \
 BlasLapack/dtrsv.o \
 BlasLapack/dzasum.o \
 BlasLapack/dzsum1.o \
 BlasLapack/idamax.o \
 BlasLapack/ieeeck.o \
 BlasLapack/ilaenv.o \
 BlasLapack/izamax.o \
 BlasLapack/izmax1.o \
 BlasLapack/lsame.o \
 BlasLapack/xerbla.o \
 BlasLapack/zaxpy.o \
 BlasLapack/zcopy.o \
 BlasLapack/zdotc.o \
 BlasLapack/zdotu.o \
 BlasLapack/zdrscl.o \
 BlasLapack/zdscal.o \
 BlasLapack/zgecon.o \
 BlasLapack/zgemm.o \
 BlasLapack/zgeru.o \
 BlasLapack/zgetf2.o \
 BlasLapack/zgetrf.o \
 BlasLapack/zlacon.o \
 BlasLapack/zladiv.o \
 BlasLapack/zlaswp.o \
 BlasLapack/zlatrs.o \
 BlasLapack/zscal.o \
 BlasLapack/zswap.o \
 BlasLapack/ztrsm.o \
 BlasLapack/ztrsv.o 


.cpp.o:
	$(CXX) $(CXXFLAGS) -o $*.o -c $*.cpp

.f.o:
	$(FC) $(FCFLAGS) -o $*.o -c $*.f

libBlasLapack: $(BLASOBJS)
	$(AR) libBlasLapack.a $(BLASOBJS)
	$(RANLIB) libBlasLapack.a
	$(MV) libBlasLapack.a ./Lib

libNDG: $(OBJS)
	$(AR) $@.a $(OBJS)
	$(RANLIB) $@.a
	$(MV) $@.a ./Lib

libINS: $(INSOBJS)
	$(AR) $@.a $(INSOBJS)
	$(RANLIB) $@.a
	$(MV) $@.a ./Lib

libMAX: $(MAXOBJS)
	$(AR) $@.a $(MAXOBJS)
	$(RANLIB) $@.a
	$(MV) $@.a ./Lib

libARBC: $(ARBCOBJS)
	$(AR) $@.a $(ARBCOBJS)
	$(RANLIB) $@.a
	$(MV) $@.a ./Lib

libT3D: $(T3DOBJS)
	$(AR) $@.a $(T3DOBJS)
	$(RANLIB) $@.a
	$(MV) $@.a ./Lib

libEUL: $(EULOBJS)
	$(AR) $@.a $(EULOBJS)
	$(RANLIB) $@.a
	$(MV) $@.a ./Lib


Maxwell2D: libNDG libMAX libBlasLapack
	$(LD) $(CXXFLAGS) -o bin/Maxwell2D Src/Examples2D/Maxwell2D/Maxwell2D_main.cpp -L./Lib -lMAX -lNDG $(BLASLAPACKLIBS) -lm 

MaxwellCurved2D: libNDG libMAX libBlasLapack
	$(LD) $(CXXFLAGS) -o bin/MaxwellCurved2D Src/Examples2D/MaxwellCurved2D/MaxwellCurved2D_main.cpp -L./Lib -lMAX -lNDG $(BLASLAPACKLIBS) -lm

MaxwellNonCon2D: libNDG libMAX libBlasLapack
	$(LD) $(CXXFLAGS) -o bin/MaxwellNonCon2D Src/Examples2D/MaxwellNonCon2D/MaxwellNonCon2D_main.cpp -L./Lib -lMAX -lNDG $(BLASLAPACKLIBS) -lm

ARBC2D: libNDG libARBC libBlasLapack
	$(LD) $(CXXFLAGS) -o bin/ARBC2D Src/Examples2D/MaxwellARBC/ARBCplane2D_main.cpp -L./Lib  $(BLASLAPACKLIBS) -lARBC -lNDG $(BLASLAPACKLIBS) -lm

CurvedINS2D: libNDG libINS libBlasLapack
	$(LD) $(CXXFLAGS) -o bin/CurvedINS2D Src/Examples2D/CurvedINS2D/CurvedINS2D_main.cpp -L./Lib -lINS -lNDG $(BLASLAPACKLIBS) -lm

Euler2D: libEUL libNDG libBlasLapack
	$(LD) $(CXXFLAGS) -o bin/Euler2D Src/Examples2D/CurvedEuler2D/CurvedEuler2D_main.cpp -L./Lib -lEUL -lNDG $(BLASLAPACKLIBS) -lm

clean:
	rm -f $(OBJS) 
	rm -f $(EULOBJS) 
	rm -f $(EULOBJS3D) 
	rm -f $(CNSOBJS) 
	rm -f $(INSOBJS) 
	rm -f $(MAXOBJS) 
	rm -f $(ARBCOBJS)
	rm -f $(SHOCKOBJS) 
	rm -f $(T3DOBJS)

realclean: clean
	rm -rf *.o ./Lib/libNDG.a ./Lib/libMAX.a ./Lib/libINS.a ./Lib/libARBC.a ./Lib/libBlasLapack.a
	($(CD) BlasLapack; rm -rf *.o )
	($(CD) Src/Arrays; rm -rf *.o )
	($(CD) Src/Codes1D; rm -rf *.o )
	($(CD) Src/Codes2D; rm -rf *.o )
	($(CD) Src/Examples2D/Maxwell2D; rm -rf *.o )
	($(CD) Src/Examples2D/MaxwellCurved2D; rm -rf *.o )
	($(CD) Src/Examples2D/CurvedINS2D; rm -rf *.o )
	($(CD) Src/ServiceRoutines; rm -rf *.o )
	($(CD) Src/Sparse; rm -rf *.o )
