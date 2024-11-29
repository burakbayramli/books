
source("SpaceFrameElement.R");

A=20e-3;moment=c(200e-6,100e-6);J=50e-6;E=210e9;G=84e9;
e1coord=c(0,0,0,3,0,0);e2coord=c(0,0,0,0,0,-3);e3coord=
c(0,0,0,0,-4,0)
k1=SpaceFrame_Element_Matrix(12,c(E,G),A,J,moment,e1coord);
k1/10^6
k2=SpaceFrame_Element_Matrix(12,c(E,G),A,J,moment,e2coord);
k2/10^6
k3=SpaceFrame_Element_Matrix(12,c(E,G),A,J,moment,e3coord);
k3/10^6

n_nodes=4;Total_dof=n_nodes*6;
K1=SpaceFrame_ExpandedElement_Matrix(Total_dof,k1,1,2);
K2=SpaceFrame_ExpandedElement_Matrix(Total_dof,k2,1,3);
K3=SpaceFrame_ExpandedElement_Matrix(Total_dof,k3,1,4);

GlobalK=K1+K2+K3;GlobalK

vec_rows_of_knownloads=c(1,2,3,4,5,6);
ReducedK=SpaceFrame_ReducedStiffnessMatrix(GlobalK,vec_rows_of_knownloads);
ReducedK

vec_values_of_knownloads=c(-10000,0,20000,0,0,0);
Reducedloadvector=SpaceFrame_ReducedLoadVector(vec_values_of_knownloads);
Reducedloadvector

UnknwonNodalDisp=SpaceFrame_NodalDisplacement(ReducedK,Reducedloadvector)
UnknwonNodalDisp

allglobalNodalDisp=matrix(rep(0,Total_dof),byrow=T);
allglobalNodalDisp[vec_rows_of_knownloads]=UnknwonNodalDisp
allglobalNodalDisp

