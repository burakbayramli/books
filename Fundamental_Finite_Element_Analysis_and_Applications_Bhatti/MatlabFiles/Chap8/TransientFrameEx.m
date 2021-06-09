% Transient analysis of a plane frame
global Mf Kf Rf
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
R = zeros(dof,1); R(5)=1;

% Generate equations for each element and assemble them.
for i=1:2:elems
    con = conn(i,:);
    lm = lmm(i,:);
    [m, k, r] = TransientPlaneFrameElement(e, inertia, a, ...
        rho+ma/a, 0, 0, nodes(con,:));
    M(lm, lm) = M(lm, lm) + m;
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end
for i=2
    con = conn(i,:);
    lm = lmm(i,:);
    [m, k, r] = TransientPlaneFrameElement(e, inertia, a, ...
        rho, 0, 0, nodes(con,:));
    M(lm, lm) = M(lm, lm) + m;
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end

% Adjust for essential boundary conditions
dof = length(R);
df = setdiff(1:dof, debc);
Mf = M(df, df);
Kf = K(df, df);
Rf = R(df) - K(df, debc)*ebcVals;

% Setup and solve the resulting first order differential equations
u0 = zeros(length(Mf),1);
v0 = zeros(length(Mf),1);
[t,d] = ode23('FrameODE',[0,10],[u0; v0]);
plot(t,d(:,2)); xlabel('time'); ylabel('Disp');
title('Vertical displacement at node 2');
