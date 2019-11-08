function [mu_avg mu_var mu_err] = plith_HW9_P2(N,T)
%Homework 9, Problem 1
%Returns the average dipole of the molecule HOOH
%Written by: Sean Kessler (plith) on 10 Nov. 2008
%Last Modified: 21 Nov. 2010
%
%To run:
% >> [Ravg R6avg] = plith_HW9_P1(100000,1200)
%Runs script for 10,000 iterations at 1200 K. Change the second parameter
%for other desired values of T
%
%Inputs:
%           N = number of Monte Carlo simulation steps
%           T = temperature [=] K
%Outputs:
%           mu_avg = average dipole moment magnitude [=] elec*A
%           mu_var = estimated variance in dipole moment values
%           mu_err = expected standard error in reported value of mu
%   Script also generates a plot showing accepted positions of atoms in the
%   molecule and histograms of the distributions of values of mu


%Define/Pack Parameters
D_OH = 360e3;   %J/mol
L_H = 1.05;     %A
alpha = 1.5;    %A^-1
k_OO = 300e-20; %J A^-2
L_O = 1.6;      %A
k_theta = 1e-18;%J radian^-2
theta_0 = 1.8;  %radian
phi_0 = 1.7;    %radian
N_A = 6.022e23; %Avogadro's Number

k_B = 1.38065e-23;  %J/K, Boltzmann constant

params = [D_OH, L_H, alpha, k_OO, L_O, k_theta, theta_0, phi_0, N_A];

%Function handles
ints = @internal;
pot = @potential;
dip = @dipole;

%Equilibrium conditions, as defined in homework solution
xO2_eq = L_O;
xH1_eq = L_H*cos(theta_0);
yH1_eq = sqrt((L_H^2)-(xH1_eq^2));
xH2_eq = L_O - (L_H*cos(theta_0));
yH2_eq = L_H*cos(phi_0)*sqrt(1-(cos(theta_0)^2));
zH2_eq1 = -yH2_eq*sqrt((sec(phi_0)^2)-1);
zH2_eq2 = -zH2_eq1;

x_eq = [xH1_eq, 0, xO2_eq, xH2_eq];
y_eq = [yH1_eq, 0, 0, yH2_eq];
z_eq1 = [0, 0, 0, zH2_eq1];
z_eq2 = [0, 0, 0, zH2_eq2];

q1 = [xH1_eq, yH1_eq, xO2_eq, xH2_eq, yH2_eq, zH2_eq1];

%From previous version of this problem, calculating <R>
% [R angs] = ints(q1);
% Rval(1) = R(4);
% 
% disp('0 K (Equilibrium) value of R:');disp(Rval(1));
% disp('0 K (Equilibrium) value of <1/R^6>:');disp(Rval(1)^(-6));

mu_vals = zeros(1,N+1);
mu_0 = dip(q1,params);
mu_vals(1) = mu_0;
fprintf('\n Equilibrium value of mu: %0.7g\n',mu_0);

Vval = zeros(1,N+1);
V1 = pot(q1,params,ints);
Vval(1) = V1;


w1 = (q1(3)^2)*abs(q1(2))*exp(-V1/(k_B*T));
x(1:3) = [q1(1), q1(3), q1(4)];
y(1:3) = [q1(2), 0, q1(5)];
z(1:3) = [0, 0, q1(6)];
n_acc = 0;
for iter = 1:N
    dq = 0.1*2*(rand(1,6)-0.5);
    q2 = q1 + dq;
    V2 = pot(q2,params,ints);
    w2 = (q2(3)^2)*abs(q2(2))*exp(-V2/(k_B*T));
    if w2>w1
        q1 = q2;
        w1 = w2;
        V1 = V2;
        n_acc = n_acc + 1;
    elseif (w2/w1 > rand())
        q1 = q2;
        w1 = w2;
        V1 = V2;
        n_acc = n_acc + 1;
    end
%     [R angs] = ints(q1);
    mu_vals(iter+1) = dip(q1,params);
    Vval(iter+1) = V1;
    x((3*iter)+1:(3*iter)+3) = [q1(1), q1(3), q1(4)];
    y((3*iter)+1:(3*iter)+3) = [q1(2), 0, q1(5)];
    z((3*iter)+1:(3*iter)+3) = [0, 0, q1(6)];
end

% Rval6 = Rval.^(-6);
% Ravg = mean(Rval); R6avg = mean(Rval6);

% Calculate the average value of mu and its variance:
mu_square = mu_vals.^2;
mu_avg = mean(mu_vals); E_mu_2 = mean(mu_square);
mu_var = E_mu_2 - (mu_avg^2);
mu_err = sqrt(mu_var/N);

fprintf('\n Units of e*A\n');
fprintf('\nExpectation value of mu:  %0.7g',mu_avg);
fprintf('\nEstimated variance of mu:  %0.7g',mu_var);
fprintf('\nExpected uncertainty:  %0.7g\n',mu_err);

fprintf('\n Units of "D"\n');
fprintf('\nExpectation value of mu:  %0.7g',mu_avg/0.208194);
fprintf('\nEstimated variance of mu:  %0.7g',mu_var/0.208194);
fprintf('\nExpected uncertainty:  %0.7g\n',mu_err/0.208194);

figure(1)
clf reset;
plot3(x_eq,y_eq,z_eq1,'-bo',x_eq,y_eq,z_eq2,'--bo',...
    'LineWidth',2,'MarkerSize',9,'MarkerEdgeColor','k',...
    'MarkerFaceColor','g');
grid on;
axis square;
xlabel('x, A');ylabel('y, A');zlabel('z, A');
title('HOOH Molecule Equilibrium Geometries');
text(xH1_eq,yH1_eq,0,'  \leftarrow H1');
text(0,0,0,'  \leftarrow O1');text(xO2_eq,0,0,'  \leftarrow O2');
text(xH2_eq,yH2_eq,zH2_eq1,'  \leftarrow H2');
text(xH2_eq,yH2_eq,zH2_eq2,'  \leftarrow H2 (alt)');

f = figure(2);
clf reset;
set(f,'PaperSize',[9 3]);
subplot(2,2,1)
plot3(x_eq,y_eq,z_eq1,'-bo',x_eq,y_eq,z_eq2,'--bo',...
    'LineWidth',2,'MarkerSize',5,'MarkerEdgeColor','k','MarkerFaceColor','g');hold on;
plot3(x,y,z,'c.');hold off;
grid on;
% axis square;
%axis([-L_H, (L_H + L_O), -1.4*L_H, 1.4*L_H, -L_H, L_H]);
axis tight;
xlabel('x, A');ylabel('y, A');zlabel('z, A');
title(sprintf('HOOH Molecule at %0.4g K',T));

subplot(2,2,[2,4])
hist(mu_vals,30);
title(sprintf('Distribution of dipole moment, <|mu|>=%0.4g',mu_avg));
xlabel('|\mu|, electron-A');
axis([0 0.75 0 3000]); axis 'auto y';

% subplot(2,2,4)
% hist(Rval6,200);
% title(sprintf('Distribution of 1/R_{HH}^{6}, <1/R^{6}>=%6.4f',R6avg));
% xlabel('1/R_{HH}^{6}, A^{-6}');
% axis([0 0.05 0 3000]); axis 'auto y';

subplot(2,2,3)
hist(Vval,50);
title('Distribution of Potentials');xlabel('V, J');

disp('Number of Monte Carlo steps attempted:');disp(N);
disp('Number accepted:');disp(n_acc);

return;

function [R angs] = internal(q)
%Returns the distance and angles (internal coordinates) between atoms in
%the molecule
%Written by: Sean Kessler (plith) on 10 Nov. 2008
%Last Modified: Never
%Inputs:
%           q = [xH1, yH1, xO2, xH2, yH2, zH2], non-zero coordinates of
%           atoms in HOOH
%Outputs:
%           R(1) = R(O1H1), distance between O1 and H1 [=] A
%           R(2) = R(O1O2) [=] A
%           R(3) = R(O2H2) [=] A
%           R(4) = R(H1H2) [=] A
%           angs(1) = Theta(HOO), angle between O1-H1 and O1-O2
%           angs(2) = Theta(OOH), angle between O2-O1 and O2-H2
%           angs(3) = phi, dihedral angle between H1 and H2

%For the purposes of saving computation time, we will take it as read that
%zH1, xO1, yO1, zO1, yO2, zO2 are all zero

xH1 = q(1);
yH1 = q(2);
R(1) = sqrt((xH1^2)+(yH1^2));

xO2 = q(3);
R(2) = abs(xO2);

xH2 = q(4);
yH2 = q(5);
zH2 = q(6);
R(3) = sqrt(((xH2 - xO2)^2)+(yH2^2)+(zH2^2));
R(4) = sqrt(((xH2 - xH1)^2)+((yH2-yH1)^2)+(zH2^2));

R_H1O2 = sqrt(((xO2 - xH1)^2)+(yH1^2));
R_O1H2 = sqrt((xH2^2)+(yH2^2)+(zH2^2));

angs(1) = acos(((R(1)^2)+(R(2)^2)-(R_H1O2^2))/(2*R(1)*R(2)));
angs(2) = acos(((R(2)^2)+(R(3)^2)-(R_O1H2^2))/(2*R(2)*R(3)));
angs(3) = acos(yH1*yH2/(abs(yH1)*sqrt((yH2^2)+(zH2^2))));
return;

function V = potential(q,params,rang)
%Returns the potential energy of proposed configuration of HOOH
%Written by: Sean Kessler (plith) on 10 Nov. 2008
%Last Modified: 21 Nov. 2010
%Inputs:
%           q = [xH1, yH1, xO2, xH2, yH2, zH2], non-zero coordinates of
%               atoms in HOOH
%           params = [D_OH, L_H, alpha, k_OO, L_0, k_theta, theta_0, phi_0,
%               N_A], physical parameters of the model
%           rang = function handle to internal coordinates
%Outputs:
%           V = potential energy, J

D_OH = params(1);
L_H = params(2);
alpha = params(3);
k_OO = params(4);
L_O = params(5);
k_theta = params(6);
theta_0 = params(7);
phi_0 = params(8);
N_A = params(9);

[R angs] = rang(q);
V_O1H1 = D_OH * ((1 - exp(-alpha*(R(1)-L_H)))^2)/N_A;
V_O2H2 = D_OH * ((1 - exp(-alpha*(R(3)-L_H)))^2)/N_A;
V_phi = (2e-20)*(1+cos(angs(1)))*(1+cos(angs(2))) * ...
    ((cos(angs(3))-cos(phi_0))^2);

V = V_O1H1 + V_O2H2 + V_phi + (0.5*((k_OO*((R(2)-L_O)^2)) + ...
    (k_theta*(((angs(1) - theta_0)^2)+((angs(2) - theta_0)^2)))));
return;

function mu = dipole(x,params)
%Returns the magnitude of the dipole of proposed configuration of HOOH
%Written by: Sean Kessler (plith) on 21 Nov. 2010
%Last Modified: Never
%Inputs:
%           x = [xH1, yH1, xO2, xH2, yH2, zH2], non-zero coordinates of
%               atoms in HOOH
%           params = [D_OH, L_H, alpha, k_OO, L_0, k_theta, theta_0, phi_0,
%               N_A], physical parameters of the model
%Outputs:
%           V = potential energy, J

%Unpack
L_H = params(2);
alpha = params(3);

rO1H1 = sqrt((x(1)^2) + (x(2)^2));
rO2H2 = sqrt(((x(3) - x(4))^2)+(x(5)^2)+(x(6)^2));
qH1 = -0.25*exp(-alpha*(rO1H1 - L_H));
qH2 = -0.25*exp(-alpha*(rO2H2 - L_H));

%Rcm = [x,y,z] position of center of mass, not actually needed
% Rcm(1) = (x(1)+x(4)+(16*x(3)))/34;
% Rcm(2) = (x(2)+x(5))/34;
% Rcm(3) = x(6)/34;

%mu vector
mu_vec(1) = qH1*x(1) + qH2*(x(4)-x(3));
mu_vec(2) = qH1*x(2) + qH2*x(5);
mu_vec(3) = qH2*x(6);

mu = norm(mu_vec,2);

return