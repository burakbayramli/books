source ("EulerBeamElement.R");

YoungMod=200e9;
MomentI1 = 5e-6;MomentI2 = 2.5e-6;MomentI3 = 6.25e-7
L1 = 1;L2 = 1;L3 = 0.5;

k1=EulerBeam_Element_Matrix(4,YoungMod,MomentI1,L1);k1
k2=EulerBeam_Element_Matrix(4,YoungMod,MomentI2,L2);k2
k3=EulerBeam_Element_Matrix(4,YoungMod,MomentI3,L3);k3

n_nodes=4;Total_dof=n_nodes*2;
K1=EulerBeam_ExpandedElement_Matrix(Total_dof,k1,1,2);
K2=EulerBeam_ExpandedElement_Matrix(Total_dof,k2,2,3);
K3=EulerBeam_ExpandedElement_Matrix(Total_dof,k3,3,4);

GlobalK=K1+K2+K3;
GlobalK

vec_rows_of_knownloads=c(2,3,4,5,6);
ReducedK=EulerBeam_ReducedStiffnessMatrix(GlobalK,vec_rows_of_knownloads);
ReducedK

vec_values_of_knownloads=c(0,-50000,0,-100000,0);
ReducedLoadVector=EulerBeam_ReducedLoadVector(vec_values_of_knownloads);
ReducedLoadVector

UnknwonNodalDisp=EulerBeam_NodalDisplacement(ReducedK, ReducedLoadVector);
UnknwonNodalDisp

allglobalNodalDisp=matrix(rep(0,Total_dof),byrow=T);
allglobalNodalDisp[vec_rows_of_knownloads]=UnknwonNodalDisp
allglobalNodalDisp

GlobalLoads=EulerBeam_Global_ForcesMoments(GlobalK, allglobalNodalDisp);
GlobalLoads
