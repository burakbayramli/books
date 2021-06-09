% Space frame example
ab = 3.2; Jb = 43; Irb = 450; Isb = 32;
ac = 4; Jc = 60; Irc = 650; Isc = 54;
q = 2./12; e = 29000.; G = 11200.;
L = 10.*12; h = 12.*12;
nodes = [0, 0, 0; 0, 0, h; L/2, 0, h; 0, L/2, h];
conn=[1,2,4; 2,3,4; 2,4,3];
lmm=[1:12; 7:18; [7:12 19:24]];
n=6*length(nodes);
debc=[1,2,3,13, 17,18, 20, 22, 24]; ebcVals=zeros(length(debc),1);
K=zeros(n); R = zeros(n,1);
% Generate equations for each element and assemble them.
for i=1
    lm=lmm(i,:);
    con=conn(i,:);
    [ke rq] = SpaceFrameElement(e, G, Irc, Isc, Jc, ac, 0, 0, ...
        nodes(con,:));
    K(lm, lm) = K(lm, lm) + ke;
    R(lm) = R(lm) + rq;
end
for i=2
    lm=lmm(i,:);
    con=conn(i,:);
    [ke rq] = SpaceFrameElement(e, G, Irb, Isb, Jb, ab, 0, q, ...
        nodes(con,:));
    K(lm, lm) = K(lm, lm) + ke;
    R(lm) = R(lm) + rq;
end
for i=3
    lm=lmm(i,:);
    con=conn(i,:);
    [ke rq] = SpaceFrameElement(e, G, Irb, Isb, Jb, ab, 0, -q, ...
        nodes(con,:));
    K(lm, lm) = K(lm, lm) + ke;
    R(lm) = R(lm) + rq;
end

% Nodal solution and reactions
format short g;
d = NodalSoln(K, R, debc, ebcVals)
fa=[]; bmra=[]; bmsa=[]; bmta=[]; Vra=[]; Vsa=[];
for i=1
    lm=lmm(i,:);
    con=conn(i,:);
    [f, bmr, bms, bmt, Vr, Vs]=SpaceFrameResults(e, G, Irc, Isc, ...
        Jc, ac, 0, 0, nodes(con,:), d(lm));
    fa = [fa; f]; bmra = [bmra; bmr]; 
    bmsa = [bmsa; bms]; bmta = [bmta; bmt];
    Vra = [Vra; Vr]; Vsa = [Vsa; Vs];
end
for i=2
    lm=lmm(i,:);
    con=conn(i,:);
    [f, bmr, bms, bmt, Vr, Vs]=SpaceFrameResults(e, G, Irb, Isb, ...
        Jb, ab, 0, q, nodes(con,:), d(lm));
    fa = [fa; f]; bmra = [bmra; bmr]; 
    bmsa = [bmsa; bms]; bmta = [bmta; bmt];
    Vra = [Vra; Vr]; Vsa = [Vsa; Vs];
end
for i=3
    lm=lmm(i,:);
    con=conn(i,:);
    [f, bmr, bms, bmt, Vr, Vs]=SpaceFrameResults(e, G, Irb, Isb, ...
        Jb, ab, 0, -q, nodes(con,:), d(lm));
    fa = [fa; f]; bmra = [bmra; bmr]; 
    bmsa = [bmsa; bms]; bmta = [bmta; bmt];
    Vra = [Vra; Vr]; Vsa = [Vsa; Vs];
end
fa
bmra
bmsa
bmta
Vra
Vsa