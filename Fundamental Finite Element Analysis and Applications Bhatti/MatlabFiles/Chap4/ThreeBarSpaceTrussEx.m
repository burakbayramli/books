% Three bar space truss example
a1 = 200; a2 = 600; e = 200000; P = 20000;
nodes = 1000*[.96, 1.92, 0; -1.44, 1.44, 0; 0, 0, 0; 0, 0, 2]; 
dof=3*length(nodes);
conn=[1,4; 2,4; 3,4];
lmm = [1, 2, 3, 10, 11, 12; 
    4, 5, 6, 10, 11, 12; 
    7, 8, 9, 10, 11, 12];
debc = [1:9];
ebcVals = zeros(length(debc),1);

%load vector
R = zeros(dof,1); R(11) = -P; 

% Assemble global stiffness matrix
K=zeros(dof);
for i=1:2
    lm=lmm(i,:);
    con=conn(i,:);
    k=SpaceTrussElement(e, a1, nodes(con,:));
    K(lm, lm) = K(lm, lm) + k;
end
lm=lmm(3,:);
con=conn(3,:);
k=SPaceTrussElement(e, a2, nodes(con,:));
K(lm, lm) = K(lm, lm) + k

% Nodal solution and reactions
[d, reactions] = NodalSoln(K, R, debc, ebcVals)
results=[];
for i=1:2
    results = [results; SpaceTrussResults(e, a1, ...
            nodes(conn(i,:),:), d(lmm(i,:)))];
end
results = [results; SpaceTrussResults(e, a2, ...
        nodes(conn(3,:),:), d(lmm(3,:)))];
format short g
results
