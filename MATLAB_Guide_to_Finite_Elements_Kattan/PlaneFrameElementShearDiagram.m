function y = PlaneFrameElementShearDiagram(f, L)
%PlaneFrameElementShearDiagram   This function plots the shear force 
%                                diagram for the plane frame element
%                                with nodal force vector f and length 
%                                L.
x = [0 ; L];
z = [f(2) ; -f(5)];
hold on;
title('Shear Force Diagram');
plot(x,z);
y1 = [0 ; 0];
plot(x,y1,'k')







