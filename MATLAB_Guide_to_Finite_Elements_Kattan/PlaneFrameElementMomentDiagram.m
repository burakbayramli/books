function y = PlaneFrameElementMomentDiagram(f, L)
%PlaneFrameElementMomentDiagram   This function plots the bending
%                                 moment diagram for the plane frame
%                                 element with nodal force vector f 
%                                 and length L.
x = [0 ; L];
z = [-f(3) ; f(6)];
hold on;
title('Bending Moment Diagram');
plot(x,z);
y1 = [0 ; 0];
plot(x,y1,'k')







