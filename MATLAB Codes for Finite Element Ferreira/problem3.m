E = 70000; A = 200; k = 2000;

numberElements = 3;
numberNodes = 4;
elementNodes = [1 2; 2 3; 3 4];
nodeCoordinates = [0 2000 4000 4000];
xx = nodeCoordinates;

displacements = zeros(numberNodes,1);
force = zeros(numberNodes,1);
stiffness = zeros(numberNodes,numberNodes);
% applied load at node 2
force(2) = 8000;
% computation of the system stiffness matrix
ea = zeros(1,numberElements);
for e = 1:numberElements
% elementDof: element degrees of freedom (Dof)
elementDof = elementNodes(e,:);
L = nodeCoordinates(elementDof(2)) ...
- nodeCoordinates(elementDof(1));
if e < 3
ea(e) = E*A/L;
else
ea(e) = k;
end
stiffness(elementDof,elementDof) = ...
stiffness(elementDof,elementDof) + ea(e)*[1 -1;-1 1];
end
prescribedDof = [1;4];
% free Dof: activeDof
activeDof = setdiff((1:numberNodes)',prescribedDof);
% solution
displacements = solution(numberNodes,prescribedDof,stiffness,force);
% output displacements/reactions
outputDisplacementsReactions(displacements,stiffness,...
numberNodes,prescribedDof)
