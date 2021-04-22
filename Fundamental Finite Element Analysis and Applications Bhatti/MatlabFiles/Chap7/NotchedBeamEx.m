% Plane stress analysis of a notched beam
e = 3000*10^3; nu = 0.2; h = 4; q = 50;
nodes = [0, 5; 0, 12; 6, 0; 6, 5; 20, 0; 20, 12; 54, 0; 54, 12];
conn = [1, 4, 6, 2; 3, 5, 6, 4; 5, 7, 8, 6];  
bx=0; by=0; alpha=0; deltaT = 0;
nel=size(conn,1); dof=2*size(nodes,1);
lmm=[];
for i=1:nel
    lm=[];
    for j=1:4
        lm=[lm, [2*conn(i,j)-1,2*conn(i,j)]];
    end
    lmm=[lmm; lm];
end
K=zeros(dof); R = zeros(dof,1);
% Generate equations for each element and assemble them.
for i=1:3
    con = conn(i,:);
    lm = lmm(i,:);
    [k, r] = PlaneQuad4Element(1, e, nu, h, alpha, deltaT, bx, by, nodes(con,:));
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end
% Add the distributed load contributions
for i=1:2:3
    con = conn(i,:);
    lm = lmm(i,:);
    r = PlaneQuad4Load(3, -q, 0, h, nodes(con,:));
    R(lm) = R(lm) + r;
end
% Nodal solution and reactions
debc = [1,3,13,14,15,16]; ebcVals=zeros(length(debc),1);
[d, reactions] = NodalSoln(K, R, debc, ebcVals)
for i=1:3
    fprintf(1,'Results for element %3.0g \n',i)
    EffectiveStress=PlaneQuad4Results(1, e, nu, alpha, deltaT, ...
        nodes(conn(i,:),:), d(lmm(i,:)))
end