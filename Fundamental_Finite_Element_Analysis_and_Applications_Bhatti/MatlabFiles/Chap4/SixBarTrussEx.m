% Six bar truss example
e = 200*10^3; A = 0.001*1000^2; P = 20000.; 
alpha = pi/6; 
nodes = 1000*[0, 0; 4, 0; 0, 3; 4, 3; 2, 2]; 
dof=2*length(nodes);
conn=[1,2; 2,5; 5,3; 2,4; 1,5; 5,4];
lmm = [1, 2, 3, 4; 3, 4, 9, 10; 9, 10, 5, 6;
    3, 4, 7, 8; 1, 2, 9, 10; 9, 10, 7, 8];
elems=size(lmm,1);
K=zeros(dof); R = zeros(dof,1);
debc = [1, 2, 5, 6, 7, 8];
ebcVals = zeros(length(debc),1);

%load vector
R = zeros(dof,1); R(3) = P*sin(alpha); R(4) = P*cos(alpha); 

% Assemble global stiffness matrix
K=zeros(dof);
for i=1:elems
    lm=lmm(i,:);
    con=conn(i,:);
    k=PlaneTrussElement(e, A, nodes(con,:));
    K(lm, lm) = K(lm, lm) + k;
end
K
R
% Nodal solution and reactions
[d, reactions] = NodalSoln(K, R, debc, ebcVals)
results=[];
for i=1:elems
    results = [results; PlaneTrussResults(e, A, ...
            nodes(conn(i,:),:), d(lmm(i,:)))];
end
format short g
results
