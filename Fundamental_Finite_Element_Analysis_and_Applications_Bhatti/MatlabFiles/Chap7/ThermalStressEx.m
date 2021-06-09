% Plane stress model for thermal stresses example
e1 = 70000; nu1 = .33; alpha1 = 23*10^(-6);
e2 = 200000; nu2 = .3; alpha2 = 12*10^(-6); h = 5;
bx=0; by=0; deltaT = 70;
a = 150/2; b = 80/2; c = 100/2; d = 30/2;
nodes = [0, 0; c, 0; a,0; 0, d; c, d; a, d; 0, b; c, b; a, b];
conn = [1, 5, 4; 1, 2, 5; 2, 6, 5;
    2, 3, 6; 4, 8, 7; 4, 5, 8; 5, 9, 8; 5, 6, 9];
nel=size(conn,1); dof=2*size(nodes,1);
lmm=[];
for i=1:nel
    lm=[];
    for j=1:3
        lm=[lm, [2*conn(i,j)-1,2*conn(i,j)]];
    end
    lmm=[lmm; lm];
end
K=zeros(dof); R = zeros(dof,1);
% Generate equations for each element and assemble them.
for i=1:2
    con = conn(i,:);
    lm = lmm(i,:);
    [k, r] = PlaneTriElement(1, e1, nu1, h, alpha1, deltaT, bx, by, nodes(con,:));
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end
for i=3:nel
    con = conn(i,:);
    lm = lmm(i,:);
    [k, r] = PlaneTriElement(1, e2, nu2, h, alpha2, deltaT, bx, by, nodes(con,:));
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end

% Nodal solution and reactions
debc = [1,2,4,6,7,13]; ebcVals=zeros(length(debc),1);
[d, reactions] = NodalSoln(K, R, debc, ebcVals)
for i=1:2
    fprintf(1,'Results for element %3.0g \n',i)
    EffectiveStress=PlaneTriResults(1, e1, nu1, alpha1, deltaT, ...
        nodes(conn(i,:),:), d(lmm(i,:)))
end
for i=3:nel
    fprintf(1,'Results for element %3.0g \n',i)
    EffectiveStress=PlaneTriResults(1, e2, nu2, alpha2, deltaT, ...
        nodes(conn(i,:),:), d(lmm(i,:)))
end