source ("AxisymmetricTriangularElement.R");

e1coord=c(0.04,0,0.08,0,0.08,0.01)
e2coord=c(0.08,0.01,0.04,0.01,0.04,0)
E1=E2=200e9;Nu=0.3

k1=AxisymmetricT_Element_Matrix(6,E1,Nu,e1coord);k1
k2=AxisymmetricT_Element_Matrix(6,E2,Nu,e2coord);k2

k2sf=AxisymmetricT_SF(6,2e6,0,1,e2coord);k2sf

n_nodes=4;Total_dof=n_nodes*2;
K1=AxisymmetricT_ExpandedElement_Matrix(Total_dof,k1,1,2,3);K1
K2=AxisymmetricT_ExpandedElement_Matrix(Total_dof,k2,3,4,1);K2
K2SF=AxisymmetricT_ExpandedSFBF(Total_dof,k2sf,3,4,1);K2SF

GlobalK=K1+K2;GlobalK

vec_rows_of_knownloads=c(1,7);
ReducedK=AxisymmetricT_ReducedStiffnessMatrix(GlobalK,vec_rows_of_knownloads);
ReducedK

vec_values_of_knownloads=c(0,0)+K2SF[vec_rows_of_knownloads]
Reducedloadvector=AxisymmetricT_ReducedLoadVector(vec_values_of_knownloads);
Reducedloadvector

UnknwonNodalDisp=AxisymmetricT_NodalDisplacement(ReducedK,Reducedloadvector)
UnknwonNodalDisp

