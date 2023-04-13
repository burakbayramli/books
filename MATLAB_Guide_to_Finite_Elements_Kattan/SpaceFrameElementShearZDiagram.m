function y = SpaceFrameElementShearZDiagram(f, L)
%SpaceFrameElementShearZDiagram   This function plots the shear force 
%                                 diagram for the space frame element
%                                 with nodal force vector f and 
%                                 length L.
x = [0 ; L];
z = [f(3) ; -f(9)];
hold on;
title('Shear Force Diagram in Z Direction');
plot(x,z);
y1 = [0 ; 0];
plot(x,y1,'k')







