source ("PlanarFrameElement.R");

E1=E2=E3=200e9;A1=A2=A3=2e-2;I1=I2=I3=350e-6
L1T1=PlaneFrame_LengthOrientation(c(0,0,3,4));L1T1
L2T2=PlaneFrame_LengthOrientation(c(3,4,5,4));L2T2
L3T3=PlaneFrame_LengthOrientation(c(5,4,7,4));L3T3

k1=PlaneFrame_Element_Matrix(6,E1,A1,I1,L1T1[1],L1T1[2]);k1
k2=PlaneFrame_Element_Matrix(6,E2,A2,I2,L2T2[1],L2T2[2]);k2
k3=PlaneFrame_Element_Matrix(6,E3,A3,I3,L3T3[1],L3T3[2]);k3

k1UDL=PlaneFrameUDL_Matrix(6,0,12000,L1T1[1],L1T1[2]);k1UDL

n_nodes=4;Total_dof=n_nodes*3;
K1=PlaneFrame_ExpandedElement_Matrix(Total_dof,k1,1,2);
K2=PlaneFrame_ExpandedElement_Matrix(Total_dof,k2,2,3);
K3=PlaneFrame_ExpandedElement_Matrix(Total_dof,k3,3,4);
K1UDL=PlaneFrameUDL_ExpandedMatirx(Total_dof,k1UDL,1,2);

GlobalK=K1+K2+K3;GlobalK

vec_rows_of_knownloads=c(3,4,5,6,7,8,9,12);
ReducedK=PlaneFrame_ReducedStiffnessMatrix(GlobalK,vec_rows_of_knownloads);
ReducedK

vec_values_of_knownloads=c(0,0,0,0,0,-20000,0,0)+K1UDL[c(3,4,5,6,7,8,9,12)];
Reducedloadvector=PlaneFrame_ReducedLoadVector(vec_values_of_knownloads);
Reducedloadvector

UnknwonNodalDisp=PlaneFrame_NodalDisplacement(ReducedK,
Reducedloadvector)
UnknwonNodalDisp

allglobalNodalDisp=matrix(rep(0,Total_dof),byrow=T);
allglobalNodalDisp[vec_rows_of_knownloads]=UnknwonNodalDisp
allglobalNodalDisp

concentratedLoads=PlaneFrame_GlobalForcesMoments(GlobalK, allglobalNodalDisp)
equivalentLoads=K1UDL
globalLoads=concentratedLoads-equivalentLoads
globalLoads

