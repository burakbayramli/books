% Modal analysis of a plane truss
g = 386.4; e = 30*10^6; A = 1.25; 
rho = (490/(12^3))/g; ma = 2000/g;
nodes = 12 * [0, 0; 10, 0; 0, 15; 20, 15; 10, 10];
conn = [3, 4; 2, 5; 5, 3; 1, 5; 5, 4];
elems = size(conn,1);
lmm=[];
for i=1:elems
    lmm = [lmm; [2*conn(i,1)-1, 2*conn(i,1),2*conn(i,2)-1, 2*conn(i,2)]];
end
debc = [1:6]; ebcVals=zeros(length(debc),1);
dof=2*size(nodes,1); 
M=zeros(dof); K=zeros(dof);
% Add nodal masses to the global M matrix.

M(7,7)=ma; M(8,8) = ma;
% Generate equations for each element and assemble them.
for i=1:elems
    con = conn(i,:);
    lm = lmm(i,:);
    [m, k] = TransientPlaneTrussElement(e, A, rho, nodes(con,:));
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