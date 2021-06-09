clear; close all
E = 30e6; A = 1; EA = E*A; L = 90; p = 50;
% generation of coordinates and connectivities
% numberElements: number of elements

numberElements = 3;

% generation equal spaced coordinates
nodeCoordinates = linspace(0,L,numberElements+1);
xx = nodeCoordinates;
% numberNodes: number of nodes
numberNodes = size(nodeCoordinates,2);
% elementNodes: connections at elements
ii = 1:numberElements;
elementNodes(:,1) = ii;
elementNodes(:,2) = ii+1;

displacements = zeros(numberNodes,1);
force = zeros(numberNodes,1);
stiffness = zeros(numberNodes,numberNodes);
% computation of the system stiffness matrix and force vector
for e = 1:numberElements
  elementDof = elementNodes(e,:);
  nn = length(elementDof);
  length_element = nodeCoordinates(elementDof(2)) ...
		   -nodeCoordinates(elementDof(1));
  detJacobian = length_element/2;
  invJacobian = 1/detJacobian;

  [shape,naturalDerivatives] = shapeFunctionL2(0.0);
  Xderivatives = naturalDerivatives*invJacobian;

  B = zeros(1,nn); B(1:nn) = Xderivatives(:);
  stiffness(elementDof,elementDof) = ...
  stiffness(elementDof,elementDof) + B'*B*2*detJacobian*EA;
  force(elementDof,1) = ...
  force(elementDof,1) + 2*shape*p*detJacobian;
end

% prescribed dofs
prescribedDof = find(xx==min(nodeCoordinates(:)) ...
| xx==max(nodeCoordinates(:)))';
% free Dof: activeDof
activeDof = setdiff((1:numberNodes)',prescribedDof);
% solution
GDof = numberNodes;
displacements = solution(GDof,prescribedDof,stiffness,force);
% output displacements/reactions
outputDisplacementsReactions(displacements,stiffness,...
numberNodes,prescribedDof)
% stresses at elements
sigma = zeros(numberElements,1);
for e = 1:numberElements
  elementDof = elementNodes(e,:);
  nn = length(elementDof);
  length_element = nodeCoordinates(elementDof(2)) ...
		   -nodeCoordinates(elementDof(1));
  sigma(e) = E/length_element*([-1 1]*displacements(elementDof));
end
% drawing nodal displacements
figure; axes1 = axes; hold on; box on; % displacements figure
plot(axes1,nodeCoordinates,displacements,...
'ok','markersize',8,'linewidth',1.5)
figure; axes2 = axes; hold on; box on; % stresses figure
% graphical representation with interpolation for each element
interpNodes = 10;
for e = 1:numberElements
  nodeA = elementNodes(e,1); nodeB = elementNodes(e,2);
  XX = linspace(nodeCoordinates(nodeA),nodeCoordinates(nodeB),...
		interpNodes);
  ll = nodeCoordinates(nodeB)-nodeCoordinates(nodeA);

  xi = (XX - nodeCoordinates(nodeA))*2/ll - 1;

  phi1 = 0.5*(1 - xi); phi2 = 0.5*(1 + xi);

  u = phi1*displacements(nodeA) + phi2*displacements(nodeB);
  plot(axes1,XX,u,'-k','linewidth',1.5)
  plot(axes1,XX,p*L*XX/2/EA.*(1 - XX/L),'--b','linewidth',1.5)

  sigma = E/ll * ones(1,interpNodes) * ...
	  (displacements(nodeB) - displacements(nodeA));
  plot(axes2,XX,sigma,'-k','linewidth',1.5)
  plot(axes2,XX,p*L/A*(0.5 - XX/L),'--b','linewidth',1.5)
end

set(axes1,'fontsize',18); set(axes2,'fontsize',18);

xlim(axes1,[0 L]); xlim(axes2,[0 L])


