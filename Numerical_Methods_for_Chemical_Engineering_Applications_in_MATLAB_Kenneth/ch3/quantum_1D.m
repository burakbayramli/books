% quantum_1D.m
% This MATLAB program computes numerically the
% ground state and lowest energy exicited states
% of a single electron in a 1-D crystal with
% square well potentials. A plane wave basis
% set is used, and the potential and wavefunction
% are assumed to have a periodicity of 2P.
% All quantities have units in a system in which
% h_bar (Planck's constant divided by 2*pi) is one
% and the mass of an electron is one.
%
% Written by Kenneth J. Beers
% MIT Department of Chemical Engineering
% March 11, 2005

function iflag_main = quantum_1D();
iflag_main = 0;

disp(' ');
disp('running quantum_1D ...');

% ask the user to input the parameters
Param = input_Param();

% Compute S and H matrix elements and
% also a plot of the external potential
[S,H, V_x] = set_matrices(Param);

% compute the lowest lying energy states
OPTS.disp = 0;
[W,D] = eigs(H, S, Param.NS, 'SR', OPTS);
e = real(diag(D));

% make a plot of the energy well density
figure;
plot(Param.x, V_x);
xlabel('x');  ylabel('External potential');
title('1-D quantum calculation');

% plot the ground state electron density
rho_e = calc_state_density(1,W,Param);
figure;  plot(Param.x,rho_e);
xlabel('x');  ylabel('electron density');
title(['Ground state, E = ', num2str(e(1))]);

% if we are calculating excited states,
% make a master plot showing the ground
% state and all exicted states
fig_master = figure;
plot(Param.x,rho_e);
xlabel('x');  ylabel('electron density');
title('1-D quantum calculation');
hold on;

% do the same for each exicted state
for k = 2 : Param.NS
    rho_e = calc_state_density(k,W,Param);
    figure;
    plot(Param.x,rho_e);
    xlabel('x');  ylabel('electron density');
    title(['Excited state # ', int2str(k), ...
        ', E = ', num2str(e(k))]);
    % add plot to master figure
    figure(fig_master);
    plot(Param.x,rho_e,'-.');    
end

iflag_main = 1;
return;


% ==============================
% This routine asks the user to
% input the parameters for the calculation.
function Param = input_Param();

disp(' ');
disp('The system is assumed to be periodic with');
disp('a spatial period of 2P');
Param.P = input('Enter P : ');

disp(' ');
disp('w is the width of the energy well');
Param.w = input('Enter w : ');

disp(' ');
disp('E_well is the depth of the energy well');
Param.E_well = input('Enter E_well : ');

disp(' ');
disp('N is the truncation order for the basis set expansion : ');
Param.N = input('Enter N : ');
% from this value, compute total number of basis functions
Param.B = 2*Param.N + 1;

% estimate the grid point spacing necessary to
% compute accurate integrals using quadrature
dx_est = (10/360)*Param.P/Param.N/pi;
% from this compute the number of grid points
% used in numerical quadrature and the grid
% point spacing
Param.nG = ceil(1 + 2*Param.P/dx_est);
Param.x = linspace(0,2*Param.P,Param.nG)';
Param.dx = Param.x(2)-Param.x(1);

disp(' ');
disp('NS is the # of lowest energy states to compute');
Param.NS = input('Enter NS : ');

return;



% ==============================
% This routine computes the elements
% of the overlap matrix S and the
% Hamiltonian matrix H
function [S,H,V_x] = set_matrices(Param);

% Set overlap matrix
S = 2*Param.P*speye(Param.B);

% set T matrix
T = spalloc(Param.B,Param.B,Param.B);
for p=1:Param.B
    m = p - Param.N - 1;
    T(p,p) = (m*pi)^2/Param.P;
end

% compute elements of V matrix using
% quadrature
V = zeros(Param.B,Param.B);

% compute potential value at each x
V_x = calc_V(Param);

% for each element of V, compute the
% element using quadrature
for p = 1:Param.B
    m = p - Param.N - 1;
    q_m = m*pi/Param.P;
    chi_m = exp(i*q_m.*Param.x);
for q = 1:Param.B
    n = q - Param.N - 1;
    q_n = n*pi/Param.P;
    chi_n_conj = exp(-i*q_n.*Param.x);
    % compute the integrand
    f = chi_m.*V_x.*chi_n_conj;
    % compute the element using the
    % trapezoid quadrature rule
    V(p,q) = trapz(Param.x,f);
end
end

% add components to get Hamiltonian matrix
H = T + V;

return;


% ==============================
% This routine computes the value of the
% external potential at each x value
function V = calc_V(Param);

V = zeros(Param.nG,1);
k_well = find( (Param.x >= (Param.P-Param.w/2)) & ...
               (Param.x <= (Param.P+Param.w/2)) );
V(k_well) = -Param.E_well;

return;


% ===============================
% This routine computes the electron
% density for the specified state k.
function rho_e = calc_state_density(k,W,Param);

% for each x, compute the wavefunction
psi = zeros(Param.nG,1);
for p = 1 : Param.B
    m = p - Param.N - 1;
    q_m = m*pi/Param.P;
    chi_m = exp(i*q_m.*Param.x);
    psi = psi + W(p,k)*chi_m;
end

% compute the resulting electron density
rho_e = conj(psi).*psi;
% and normalize to one
rho_int = trapz(Param.x,rho_e);
rho_e = rho_e./rho_int;

return;


