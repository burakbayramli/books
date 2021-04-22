% Solution of poisson equation example using
% two 8 node elements (symmetric left half model)
kx = 1;  ky = 1; p=0; q=0;
nodes=[0,0; 0,1/4; 0,1/2; 0,3/4;
    0,1; 1/4,0; 1/4,1/2; 1/4,1; 1/2,0; 1/2,1/4; 
    1/2,1/2; 1/2,3/4; 1/2,1];
lmm=[1,6,9,10,11,7,3,2;
    3,7,11,12,13,8,5,4];
debc=[1,2,3,4,5,6,8,9,13]';
ebcVals=[0,0,0,0,0,3/16,0,1/4,0]';
dof=length(nodes); elems=size(lmm,1);
K=zeros(dof); R = zeros(dof,1);
% Generate equations for each element and assemble them.
for i=1:elems
    lm = lmm(i,:);
    [k, r] = BVPRect8Element(kx, ky, p, q, nodes(lm,:));
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end
% Nodal solution
d = NodalSoln(K, R, debc, ebcVals)
results=[];
for i=1:elems
    results = [results; BVPRect8Results(nodes(lmm(i,:),:), d(lmm(i,:)))];
end
results
