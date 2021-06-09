% % Transient heat flow through V-groove
global Mf Kf Rf
h = 200; Tinf = 50; rho = 1600; cp = 800; t0=300;
nodes = (1/100)* [0, 0; 0, 1; 0, 2;
    1, 0; 1, 3/2; 1, 3; 2, 0; 2, 2; 2, 4];
kx = 3; ky = 3; p = 0; q = 0;
lmm = [1, 4, 5; 5, 2, 1; 2, 5, 6;
    6, 3, 2; 4, 7, 8; 8, 5, 4; 5, 8, 9; 9, 6, 5];
debc = [3,6,9]; ebcVals=t0*ones(length(debc),1);
dof=length(nodes); elems=size(lmm,1);
M=zeros(dof); K=zeros(dof); R = zeros(dof,1);

% Generate equations for each element and assemble them.
for i=1:elems
    lm = lmm(i,:);
    [m, k, r] = TransientBVPTriElement(kx, ky, p, q, rho*cp, nodes(lm,:));
    M(lm, lm) = M(lm, lm) + m;
    K(lm, lm) = K(lm, lm) + k;
    R(lm) = R(lm) + r;
end

% Compute and assemble NBC contributions
lm = lmm(1,:);
[k, r] = BVPTriNBCTerm(1, -h, h*Tinf, nodes(lm,:));
K(lm, lm) = K(lm, lm) + k;
R(lm) = R(lm) + r;

lm = lmm(5,:);
[k, r] = BVPTriNBCTerm(1, -h, h*Tinf, nodes(lm,:));
K(lm, lm) = K(lm, lm) + k;
R(lm) = R(lm) + r;
% Adjust for essential boundary conditions
dof = length(R);
df = setdiff(1:dof, debc);
Mf = M(df, df);
Kf = K(df, df);
Rf = R(df) - K(df, debc)*ebcVals;

% Setup and solve the resulting first order differential equations
d0 = t0*ones(length(Mf),1);
[t,d] = ode23('HeatODE',[0,300],d0);
plot(t,d(:,1),'-r',t,d(:,2),'-g',t,d(:,3),...
    '-c',t,d(:,4),'-m',t,d(:,5),'-y',t,d(:,6),'-k');
legend('T1', 'T2', 'T4', 'T5', 'T7', 'T8');
hold off
[t(1:10),d(1:10,:)]