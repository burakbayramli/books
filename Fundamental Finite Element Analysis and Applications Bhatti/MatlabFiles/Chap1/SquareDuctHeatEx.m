% Heat flow through a square duct example
kx=1.4; ky=1.4; Q=0;
nodes=[0,0; 20,0; 20,30; 0,10; 10,10]/100;
lmm =[1,2,5; 2,3,5; 3,4,5; 1,5,4];

K=zeros(5); R = zeros(5,1);
% Generate equations for each element and assemble them.
for i=1:4
    lm = lmm(i,:);
    [k, r] = HeatTriElement(kx, ky, Q, nodes(lm,:));
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end
% Add the term beacuse of convection on side 1 of element 2
h=27;Tinf=20; lm = lmm(2,:);
[kh, rh] = ConvectionTerm(1,h,Tinf,nodes(lm,:));
K(lm, lm) = K(lm, lm) + kh
R(lm) = R(lm) + rh

% Nodal solution and reactions
[d, reactions] = NodalSoln(K, R, [1,4], [300; 300])
results=[];
for i=1:4
    results = [results; HeatTriResults(nodes(lmm(i,:),:), d(lmm(i,:)))];
end
results
