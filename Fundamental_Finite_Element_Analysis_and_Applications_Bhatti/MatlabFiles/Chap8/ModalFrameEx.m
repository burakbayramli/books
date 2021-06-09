% Modal analysis of a plane frame
L = 1000; e = 200000; rho = 7.84*10^(-6); a = 240; 
inertia = 2000; ma = 0.0203874;
nodes = [0, 0; L, 0; L, -L; 2*L, -L];
conn = [1,2; 2,3; 3,4];
elems = size(conn,1);
lmm=[];
for i=1:elems
    lmm = [lmm; [3*conn(i,1)-2, 3*conn(i,1)-1, 3*conn(i,1),...
                3*conn(i,2)-2, 3*conn(i,2)-1, 3*conn(i,2)]];
end
debc = [1,2,3,10,11,12]; ebcVals=zeros(length(debc),1);
dof=3*size(nodes,1); 
M=zeros(dof); K=zeros(dof);

% Generate equations for each element and assemble them.
for i=1:2:elems
    con = conn(i,:);
    lm = lmm(i,:);
    [m, k, r] = TransientPlaneFrameElement(e, inertia, a, ...
        rho+ma/a, 0, 0, nodes(con,:));
    M(lm, lm) = M(lm, lm) + m;
    K(lm, lm) = K(lm, lm) + k;
end
for i=2
    con = conn(i,:);
    lm = lmm(i,:);
    [m, k, r] = TransientPlaneFrameElement(e, inertia, a, ...
        rho, 0, 0, nodes(con,:));
    M(lm, lm) = M(lm, lm) + m;
    K(lm, lm) = K(lm, lm) + k;
end

% Adjust for essential boundary conditions
dof = length(R);
df = setdiff(1:dof, debc);
Mf = M(df, df);
Kf = K(df, df);

% Compute frequencies and mode shapes
[V, lam] = eig(Kf, Mf);
freq=sqrt(lam)
modeShapes = V