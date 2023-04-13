function y = SpaceFrameElementShearYDiagram(f, L)
%SpaceFrameElementShearYDiagram   This function plots the shear force 
%                                 diagram for the space frame element
%                                 with nodal force vector f and 
%                                 length L.
x = [0 ; L];
z = [f(2) ; -f(8)];
hold on;
title('Shear Force Diagram in Y Direction');
plot(x,z);
y1 = [0 ; 0];
plot(x,y1,'k')







