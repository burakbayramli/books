function y = SpaceFrameElementTorsionDiagram(f, L)
%SpaceFrameElementTorsionDiagram   This function plots the torsion 
%                                  diagram for the space frame 
%                                  element with nodal force vector f 
%                                  and length L.
x = [0 ; L];
z = [f(4) ; -f(10)];
hold on;
title('Torsion Diagram');
plot(x,z);
y1 = [0 ; 0];
plot(x,y1,'k')







