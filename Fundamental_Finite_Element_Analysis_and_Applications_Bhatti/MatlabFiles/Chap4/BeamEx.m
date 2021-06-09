% Beam example
F = 18; q = 10; EI = (210*10^6)*4*10^(-4);
L=2; nodes =[0:L:3*L]; n=2*length(nodes);
debc=[1,2,7]; ebcVals=zeros(length(debc),1);
K=zeros(n); R = zeros(n,1); R(3)=-F;
% Generate equations for each element and assemble them.
for i=1:2
    lm=[2*(i-1)+1,2*(i-1)+2,2*(i-1)+3,2*(i-1)+4];
    [ke, rq] = BeamElement(2*EI, 0, nodes([i:i+1]));
    K(lm, lm) = K(lm, lm) + ke;
    R(lm) = R(lm) + rq;
end
for i=3
    lm=[2*(i-1)+1,2*(i-1)+2,2*(i-1)+3,2*(i-1)+4];
    [ke, rq] = BeamElement(EI, -q, nodes([i:i+1]));
    K(lm, lm) = K(lm, lm) + ke;
    R(lm) = R(lm) + rq;
end
K
R
% Nodal solution and reactions
d = NodalSoln(K, R, debc, ebcVals)
va=[]; bma=[]; Va=[];
for i=1:2
    lm=[2*(i-1)+1,2*(i-1)+2,2*(i-1)+3,2*(i-1)+4];
    [v, bm, V]=BeamResults(2*EI, 0, nodes([i:i+1]), d(lm));
    va = [va; v]; bma = [bma; bm]; Va = [Va; V];
end
for i=3
    lm=[2*(i-1)+1,2*(i-1)+2,2*(i-1)+3,2*(i-1)+4];
    [v, bm, V]=BeamResults(EI, -q, nodes([i:i+1]), d(lm));
    va = [va; v]; bma = [bma; bm]; Va = [Va; V];
end
va
bma
Va