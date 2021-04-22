% Torsion of a C Shaped Section
kx=1; ky=1; q=2; p=0;
nodes=[0,0; 0,6; 0.255,0;0.255,5.749499999999999;
    0.51,0; 0.51,5.499; 3.17,5.499; 3.17,5.749499999999999; 3.17,6];
lmm = [1,3,4; 4,2,1; 3,5,6; 6,4,3; 6,7,8; 8,4,6; 4,8,9; 9,2,4];
debc = [1,2,5,6,7,8,9]; ebcVals=zeros(length(debc),1);
dof=length(nodes); elems=size(lmm,1);
K=zeros(dof); R = zeros(dof,1);

% Generate equations for each element and assemble them.
for i=1:elems
    lm = lmm(i,:);
    [k, r] = BVPTriElement(kx, ky, p, q, nodes(lm,:));
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end

% Nodal solution
d = NodalSoln(K, R, debc, ebcVals)
results=[];
for i=1:elems
    results = [results; BVPTriResults(nodes(lmm(i,:),:), d(lmm(i,:)))];
end
results
% Torsional constant calculations
J = 4*sum(results(:,4))