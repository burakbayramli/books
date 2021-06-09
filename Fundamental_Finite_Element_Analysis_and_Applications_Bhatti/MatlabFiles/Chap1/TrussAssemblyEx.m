% Truss Assembly Example
e1=200*10^3; e2=70*10^3; a1=40*100; a2=30*100; a3=20*100;
P = 150*10^3;
nodes = 1000*[0, 0; 1.5, 3.5; 0, 5; 5, 5];
K=zeros(8);
% Generate stiffness matrix for each element and assemble it.
k=PlaneTrussElement(e1, a1, nodes([1 2],:));
lm=[1, 2, 3, 4];
K(lm, lm) = K(lm, lm) + k

k=PlaneTrussElement(e1, a1, nodes([2 4],:));
lm=[3, 4, 7, 8];
K(lm, lm) = K(lm, lm) + k

k=PlaneTrussElement(e1, a2, nodes([1 3],:));
lm=[1, 2, 5, 6];
K(lm, lm) = K(lm, lm) + k

k=PlaneTrussElement(e1, a2, nodes([3 4],:));
lm=[5, 6, 7, 8];
K(lm, lm) = K(lm, lm) + k

k=PlaneTrussElement(e2, a3, nodes([2 3],:));
lm=[3, 4, 5, 6];
K(lm, lm) = K(lm, lm) + k

% Define the load vector
R = zeros(8,1); R(4)=-P