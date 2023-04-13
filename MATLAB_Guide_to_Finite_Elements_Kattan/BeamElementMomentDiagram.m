function y = BeamElementMomentDiagram(f, L)
%BeamElementMomentDiagram   This function plots the bending moment 
%                           diagram for the beam element with nodal
%                           force vector f and length L.
x = [0 ; L];
z = [-f(2) ; f(4)];
hold on;
title('Bending Moment Diagram');
plot(x,z);
y1 = [0 ; 0];
plot(x,y1,'k')







