source ("LinearTriangularElement.R");

e1coord=c(0,0.5,0.125,0.375,0.25,0.5,1)
e2coord=c(0,0.5,0,0.25,0.125,0.375,1)
e3coord=c(0.125,0.375,0.25,0.25,0.25,0.5,1)
e4coord=c(0.125,0.375,0,0.25,0.25,0.25,1)
e5coord=c(0,0.25,0.125,0.125,0.25,0.25,1)
e6coord=c(0,0.25,0,0,0.125,0.125,1)
e7coord=c(0.25,0.25,0.125,0.125,0.25,0,1)
e8coord=c(0.125,0.125,0,0,0.25,0,1)
e9coord=c(0.25,0.25,0.25,0,0.375,0.125,1)
e10coord=c(0.25,0.25,0.375,0.125,0.5,0.25,1)
e11coord=c(0.25,0,0.5,0,0.375,0.125,1)
e12coord=c(0.375,0.125,0.5,0,0.5,0.25,1)
YoungMod=E=210e9;
thickness=0.025;Nu=0.3;

k1=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e1coord,1);k1
k2=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e2coord,1);k2
k3=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e3coord,1);k3
k4=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e4coord,1);k4
k5=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e5coord,1);k5
k6=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e6coord,1);k6
k7=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e7coord,1);k7
k8=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e8coord,1);k8
k9=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e9coord,1);k9
k10=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e10coord,1);k10
k11=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e11coord,1);k11
k12=CSTriangular_Element_Matrix(6,E,Nu,thickness,
e12coord,1);k12

k10SurfaceForce=CSTriangular_SF(6,0,-4000e3,0.25,thickness,3);
k10SurfaceForce

n_nodes=11;Total_dof=n_nodes*2;
K1=CSTriangular_ExpandedElement_Matrix(Total_dof,k1,1,3,2);
K2=CSTriangular_ExpandedElement_Matrix(Total_dof,k2,1,4,3);
K3=CSTriangular_ExpandedElement_Matrix(Total_dof,k3,3,5,2);
K4=CSTriangular_ExpandedElement_Matrix(Total_dof,k4,3,4,5);
K5=CSTriangular_ExpandedElement_Matrix(Total_dof,k5,4,6,5);
K6=CSTriangular_ExpandedElement_Matrix(Total_dof,k6,4,7,6);
K7=CSTriangular_ExpandedElement_Matrix(Total_dof,k7,5,6,8);
K8=CSTriangular_ExpandedElement_Matrix(Total_dof,k8,6,7,8);
K9=CSTriangular_ExpandedElement_Matrix(Total_dof,k9,
5,8,9);
K10=CSTriangular_ExpandedElement_Matrix(Total_dof,k10,
5,9,10);
K11=CSTriangular_ExpandedElement_Matrix(Total_dof,k11,
8,11,9);
K12=CSTriangular_ExpandedElement_Matrix(Total_dof,k12,
9,11,10);
K10SF=CSTriangular_ExpandedSF(Total_dof,k10SurfaceForce,
5,9,10);K10SF

GlobalK=K1+K2+K3+K4+K5+K6+K7+K8+K9+K10+K11+K12;GlobalK

vec_rows_of_knownloads=c(3,4,5,6,9,10,11,12,15,16,17,18,19,20,21);
ReducedK=CSTriangular_ReducedStiffnessMatrix(GlobalK,vec_rows_of_knownloads);
ReducedK

vec_values_of_knownloads=c(rep(0,13),12.5e3,0)+K10SF	[vec_rows_of_knownloads];
Reducedloadvector=CSTriangular_ReducedLoadVector(vec_values_of_knownloads);
Reducedloadvector

UnknwonNodalDisp=CSTriangular_NodalDisplacement(ReducedK,Reducedloadvector)
UnknwonNodalDisp

allglobalNodalDisp=matrix(rep(0,Total_dof),byrow=T);
allglobalNodalDisp[vec_rows_of_knownloads]= UnknwonNodalDisp
allglobalNodalDisp

ConcglobalLoads=CSTriangular_GlobalForces(GlobalK,allglobalNodalDisp)
EquivalentSF=K10SF
globalLoads=ConcglobalLoads-EquivalentSF;
round(globalLoads,6)

Element1_Stress=CSTriangular_Stresses(E,Nu,thickness,e1coord,1,allglobalNodalDisp,1,3,3)
Element1_Stress/10^6
Element2_Stress=CSTriangular_Stresses(E,Nu,thickness,e2coord,1,allglobalNodalDisp,1,4,3)
Element2_Stress/10^6
Element3_Stress=CSTriangular_Stresses(E,Nu,thickness,e3coord,1,allglobalNodalDisp,3,5,2)
Element3_Stress/10^6
Element10_Stress=CSTriangular_Stresses(E,Nu,thickness,e4coord,1,allglobalNodalDisp,5,9,10)
Element10_Stress/10^6
