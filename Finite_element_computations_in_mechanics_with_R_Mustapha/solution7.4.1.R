source ("PlanarFrameElement.R");
E1=E2=210e9;A1=A2=2.1;I1=I2=300e-6
L1T1=PlaneFrame_LengthOrientation(c(0,4,5,4));
L2T2=PlaneFrame_LengthOrientation(c(5,4,5,0));
L1T1=PlaneFrame_LengthOrientation(c(0,4,5,4));L1T1
L2T2=PlaneFrame_LengthOrientation(c(5,4,5,0));L2T2
k1=PlaneFrame_Element_Matrix(6,E1,A1,I1,L1T1[1],L1T1[2]);k1
k2=PlaneFrame_Element_Matrix(6,E2,A2,I2,L2T2[1],L2T2[2]);k2
n_nodes=3;Total_dof=n_nodes*3;
K1=PlaneFrame_ExpandedElement_Matrix(Total_dof,k1,1,2);
K2=PlaneFrame_ExpandedElement_Matrix(Total_dof,k2,2,3);
GlobalK=K1+K2;GlobalK
vec_rows_of_knownloads=c(4:6,9);
ReducedK=PlaneFrame_ReducedStiffnessMatrix(GlobalK,vec_rows_of_knownloads);
ReducedK
