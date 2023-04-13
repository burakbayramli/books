function y = SpaceFrameElementAxialDiagram(f, L)
%SpaceFrameElementAxialDiagram   This function plots the axial force 
%                                diagram for the space frame element
%                                with nodal force vector f and length
%                                L.
x = [0 ; L];
z = [-f(1) ; f(7)];
hold on;
title('Axial Force Diagram');
plot(x,z);
y1 = [0 ; 0];
plot(x,y1,'k')







