% Five bar truss example
e1=200*10^3; e2=70*10^3; a1=40*100; a2=30*100; a3=20*100;
P = 150*10^3;
nodes = 1000*[0, 0; 1.5, 3.5; 0, 5; 5, 5];
conn = [1,2; 2,4; 1,3; 3,4; 2,3];
lmm = [1,2,3,4; 3, 4, 7, 8; 1, 2, 5, 6; 5, 6, 7, 8; 3, 4, 5, 6];  
K=zeros(8);
% Generate stiffness matrix for each element and assemble it.
for i=1:2
    lm=lmm(i,:);
    con=conn(i,:);
    k=PlaneTrussElement(e1, a1, nodes(con,:));
    K(lm, lm) = K(lm, lm) + k;
end
for i=3:4
    lm=lmm(i,:);
    con=conn(i,:);
    k=PlaneTrussElement(e1, a2, nodes(con,:));
    K(lm, lm) = K(lm, lm) + k;
end

lm=lmm(5,:); con=conn(5,:);
k=PlaneTrussElement(e2, a3, nodes(con,:));
K(lm, lm) = K(lm, lm) + k

% Define the load vector
R = zeros(8,1); R(4)=-P

% Nodal solution and reactions
[d, reactions] = NodalSoln(K, R, [1,2,7,8], zeros(4,1))
results=[];
for i=1:2
    results = [results; PlaneTrussResults(e1, a1, ...
            nodes(conn(i,:),:), d(lmm(i,:)))];
end
for i=3:4
    results = [results; PlaneTrussResults(e1, a2, ...
            nodes(conn(i,:),:), d(lmm(i,:)))];
end
format short g
results = [results; PlaneTrussResults(e2, a3, ...
        nodes(conn(5,:),:), d(lmm(5,:)))]
