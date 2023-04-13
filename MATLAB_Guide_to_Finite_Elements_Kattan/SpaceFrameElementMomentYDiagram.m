function y = SpaceFrameElementMomentYDiagram(f, L)
%SpaceFrameElementMomentYDiagram   This function plots the bending 
%                                  moment diagram for the space frame 
%                                  element with nodal force vector f 
%                                  and length L.
x = [0 ; L];
z = [f(5) ; -f(11)];
hold on;
title('Bending Moment Diagram along Y Axis');
plot(x,z);
y1 = [0 ; 0];
plot(x,y1,'k')







