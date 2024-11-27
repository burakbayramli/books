
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
