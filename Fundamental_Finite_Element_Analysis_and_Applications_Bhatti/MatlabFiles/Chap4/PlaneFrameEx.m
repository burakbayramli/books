% Plane frame example
e = 30000; a = 100; inertia = 1000; L = 15*12; q = 1/12;
nodes = [0, 0; L/sqrt(2), L/sqrt(2); L + L/sqrt(2), L/sqrt(2)];
conn=[1,2; 2,3];
lmm=[1,2,3,4,5,6; 4,5,6,7,8,9];
n=3*length(nodes);
debc=[1,2,3,7,8,9]; ebcVals=zeros(length(debc),1);
K=zeros(n); R = zeros(n,1);
% Generate equations for each element and assemble them.
for i=1
    lm=lmm(i,:);
    con=conn(i,:);
    [ke, rq] = PlaneFrameElement(e, inertia, a, 0, -q, nodes(con,:));
    K(lm, lm) = K(lm, lm) + ke;
    R(lm) = R(lm) + rq;
end
for i=2
    lm=lmm(i,:);
    con=conn(i,:);
    [ke, rq] = PlaneFrameElement(e, inertia, a, 0, 0, nodes(con,:));
    K(lm, lm) = K(lm, lm) + ke;
    R(lm) = R(lm) + rq;
end
K
R
% Nodal solution and reactions
d = NodalSoln(K, R, debc, ebcVals)
fa=[]; bma=[]; Va=[];
for i=1
    lm=lmm(i,:);
    con=conn(i,:);
    [f, bm, V]=PlaneFrameResults(e, inertia, a, 0, -q, ...
        nodes(con,:), d(lm));
    fa = [fa; f]; bma = [bma; bm]; Va = [Va; V];
end
for i=2
    lm=lmm(i,:);
    con=conn(i,:);
    [f, bm, V]=PlaneFrameResults(e, inertia, a, 0, 0, ...
        nodes(con,:), d(lm));
    fa = [fa; f]; bma = [bma; bm]; Va = [Va; V];
end
fa
bma
Va