% polymer_flow_1D.m
% This MATLAB program uses finite differences
% to model the flow rate of a poly(styrene)
% melt between two parallel flat plates,
% using the Carreau-Yasuda model of a shear
% thinning fluid.
% This version uses one-sided approximations
% for the first-derivatives to avoid oscillations
% and provides a sparsity pattern to fsolve to
% speed up Jacobian calculations.
% K.J. Beers. MIT ChE. 2/17/2005

function iflag_main = polymer_flow_1D();
iflag_main = 0;

% set the fluid parameters of poly(styrene)
% melt at 453K and plot predicted shear-rate
% dependent viscosity
Fluid.eta_0 = 1.48e4;  % in Pa*s
Fluid.eta_inf = 0;
Fluid.lambda = 1.04; % in s
Fluid.a = 2;
Fluid.n = 0.398;

% Ask the user to input the problem
% specifications
Sys.B = input(...
    'Enter distance in m between plates : ');

% Ask the user for a target mid-point velocity.
% Then, for reference, the pressure gradient
% necessary to achieve this velocity, neglecting
% shear-thinning effects, is printed to the screen.
v_tar = input(...
    'Enter target mid-point velocity in m/s : ');
dP_dx_tar = -8*Fluid.eta_0*v_tar/Sys.B/Sys.B;
disp(' ');
disp(...
    'Neglecting shear-thinning effects, the dP_dx');
disp(['necessary to achieve this velocity is : ', ...
    num2str(dP_dx_tar)]);
disp(' ');
% Then, compute approximate Reynolds' # to check
% for whether these flow conditions are at all
% consistent with laminar flow
Re_tar = 1000*v_tar*Sys.B/Fluid.eta_0;
disp(['approx. target Reynolds number : ', ...
    num2str(Re_tar)]);
disp(' ');
Sys.dP_dx = input(...
    'Enter dynamic pressure gradient in Pa/m : ');

% ask user to input grid parameters
Grid.N = input(...
    'Enter number of grid points : ');
% generate the grid data
Grid.dy = Sys.B/(Grid.N + 1);
Grid.y = linspace(Grid.dy,Sys.B-Grid.dy,Grid.N);

% Make plot of shear-dependent viscosity
gammaDotVals = logspace(-1,2,250);
viscVals = calc_viscosity(gammaDotVals,Fluid);
figure;  loglog(gammaDotVals,viscVals);
xlabel('Shear rate in s^{-1}');
ylabel('Viscosity in Pa*s');
title('Viscosity of poly(styrene) melt at 453K');

% make initial guess based on velocity profile
% of Newtonian fluid with the zero shear-rate
% viscosity.
v_0 = 1/2/Fluid.eta_0*Sys.dP_dx.* ...
    (Grid.y.*(Grid.y - Sys.B));

% generate matrix showing sparsity
% pattern of the Jacobian
Jac_S = Jac_pattern(Grid);

% set solver parameters
Options = optimset('LargeScale','off', ...
    'JacobPattern', Jac_S);
% call fsolve
[v, f] = fsolve(@calc_f, v_0, Options, ...
    Sys, Fluid, Grid);

% check infinity norm of solution vector
f_norm = norm(f,inf);
disp(['function infinity norm = ', ...
    num2str(f_norm)]);

% compute for the solution the shear rate
% and viscosity at each grid point
gamma_dot = calc_gamma_dot(v, Grid);
eta = calc_viscosity(gamma_dot, Fluid);
shear_stress = Sys.dP_dx.*(Grid.y - Sys.B/2);

% compute local Reynolds' numbers and return
% max. value as check on validity of
% laminar flow calculations
Re = 1000.*(v.^2)./eta./gamma_dot;
Re_max = max(Re);
disp(['max. local Reynolds number = ', ...
    num2str(Re_max)]);

% plot the results
figure;  subplot(2,2,1);  plot(v,Grid.y);
hold on;  plot(v_0, Grid.y, '-.');
xlabel('v_x(y) (m/s)');   ylabel('y (m)');
title('1-D laminar flow of PS melt at 453K');
subplot(2,2,2);  plot(shear_stress, Grid.y);
xlabel('\tau_{yx} (Pa)');  ylabel('y (m)');
subplot(2,2,3);  plot(gamma_dot, Grid.y);
xlabel('shear rate (s^{-1})');  ylabel('y (m)');
subplot(2,2,4);  plot(eta, Grid.y);
xlabel('viscosity (Pa*s)');  ylabel('y (m)');

% save results in .mat file
save polymer_flow_1D_results.mat;

iflag_main = 1;
return;


% ====================
% This routine estimates the shear-rate dependent
% viscosity of a fluid using the Carreau-Yasuda
% model.
function visc = calc_viscosity(gamma_dot,Fluid);

var1 = 1 + (Fluid.lambda.*gamma_dot).^Fluid.a;
var2 = var1.^((Fluid.n - 1)/Fluid.a);

visc = Fluid.eta_inf + ...
    (Fluid.eta_0 - Fluid.eta_inf).*var2;
return;

% ====================
% This routine returns a sparse matrix showing
% the locations of the non-zero elements in
% the Jacobian
function Jac_S = Jac_pattern(Grid);

Jac_S = spalloc(Grid.N,Grid.N,2*Grid.N);
for k=1:Grid.N
    Jac_S(k,k) = 1;
    if(k > 1)
        Jac_S(k,k-1) = 1;
    end
end

return;

% ====================
% This routine returns the vector of function
% values for the set of nonlinear algebraic equations.
function f = calc_f(v, Sys, Fluid, Grid);

f = zeros(Grid.N,1);
Jac = zeros(Grid.N,Grid.N);

% compute shear rates at each grid point
gamma_dot = calc_gamma_dot(v, Grid);

% compute viscosities at each grid point
eta = calc_viscosity(gamma_dot, Fluid);

var1 = 1/Grid.dy;
% compute function values
f(1) = eta(1)*v(1)*var1 ...
    - Sys.dP_dx*(Grid.y(1) - Sys.B/2);
for k=2:Grid.N
    f(k) = eta(k)*(v(k)-v(k-1))*var1 ...
        - Sys.dP_dx*(Grid.y(k) - Sys.B/2);
end

return;


% ====================
% This routine estimates the shear rates
% at each grid point.
function gamma_dot = calc_gamma_dot(v, Grid);

gamma_dot = zeros(size(v));
gamma_dot(1) = v(1);
for k=2:Grid.N
    gamma_dot(k) = v(k) - v(k-1);
end
gamma_dot = gamma_dot.*(1/Grid.dy);
gamma_dot = abs(gamma_dot);

return;

