% catalyst_nonisothermal_scan.m
%
% This MATLAB program plots the effectiveness factor vs. the 
% Thiele modulus for a catalyst pellet with first order reaction 
% at steady state in nonisothermal operation. No external mass or
% heat transfer resistance is included, and Diriclet boundary 
% conditions are employed at the particle surface. To solve the 
% dimensionless PDE, finite differences are used with a non-uniform
% grid.
%
% The dimensionless input parameters are :
% Phi_TM = Thiele modulus
%        = product of particle radius and the square root of
%          the rate constant divided by the diffusivity
%        = measure of importance of diffusion
%          limitations
% gamma = activation energy / (R*Ts), where
%         Ts is the known surface temperature
% beta = Da*(-DH)*Cs/lambda/Ts, where Da is the diffusivity,
%         DH is the heat of reaction, Cs is the known surface
%         concentration, lambda is the thermal conductivity,
%         and Ts is the known surface temperature
%      = measure of importance of heat generation
%        by reaction vs. thermal conductivity
%
% Written K. Beers. MIT ChE. 9/21/2002
function iflag_main = catalyst_nonisothermal_scan();
cpu_start = cputime;
iflag_main = 0;

% First, have the user input the simulation parameters.
disp('Enter simulation parameters, ...');
% problem parameters
beta = input('Enter beta (row vector) : ');
num_beta = length(beta);
gamma = input('Enter gamma : ');
% Thiele modulus bounds (log scale)
Phi_TM_min = input('Enter min. Thiele modulus : ');
Phi_TM_max = input('Enter max. Thiele modulus : ');
num_TM = input('Enter number of Thiele modulus points : ');
Phi_TM = logspace(log10(Phi_TM_min),log10(Phi_TM_max),num_TM);
% number of physical grid points, use simple uniform scale
% with fine grid near surface
dz = input('Enter interior grid point spacing : ');
dz_fine = input('Enter surface (0.9-1.0) spacing : ');
%z_inner = [dz : dz : 0.9-dz];
%z_outer = [0.9 : dz_fine : 1.0-dz_fine];
z_grid = [ [dz : dz : 0.9-dz], [0.9 : dz_fine : 1.0-dz_fine] ];
num_grid = length(z_grid);

% We now scan the values of the Thiele modulus, starting at the
% lowest value. For each value of the Thiele modulus, we need to
% solve the set of nonlinear algebraic equations obtained by finite
% difference disrectization of the governing PDE. We start at the
% lowest value of the Thiele modulus, and use as an initial guess a
% uniform concentation profile of 1.  In the limit as the Thiele 
% modulus goes to zero, this initial guess is correct. For each 
% subsequent calculation, we use the final result of the
% previous calculation as an initial guess.

% allocate space to store the effectiveness factor
eta_eff = zeros(num_beta,num_TM);
figure;
% iterative over various values of beta
for k_beta = 1:num_beta; 
    
    % form initial guess - uniform concentration
    phiA_guess = ones(size(z_grid));

    % set some options flags for fsolve()
    options = optimset('LargeScale','off','Jacobian','on');

    % iterate over each value of the Thiele modulus
    for i_TM = 1:num_TM
    
        % compute dimensionless profile from Newton's method
        [phiA,fval,exitflag] = ...
            fsolve(@catalyst_nonisothermal_calc_f, ...
            phiA_guess, options, ...
            Phi_TM(i_TM), beta(k_beta), gamma, z_grid);
        if(exitflag <= 0)
            error('fsolve did not converge');
        end    
        
        % compute effectiveness factor
        eta_eff(k_beta,i_TM) = calc_eta_eff(phiA, ...
            Phi_TM(i_TM), beta(k_beta), gamma, z_grid);
        
        % update initial guess
        phiA_guess = phiA;
    end

    % Now, plot the results
    semilogx(Phi_TM,eta_eff);
    gtext(num2str(beta(k_beta)));
    hold on;
end
xlabel('\Phi (Thiele modulus)');
ylabel('\eta (effectiveness factor)');
title(['Nonisothermal catalyst: ', ...
        '\gamma = ', num2str(gamma), ...
        ', \beta varied']);

cpu_end = cputime; cpu_elapsed = cpu_end - cpu_start;
disp(['Elapsed CPU time : ', num2str(cpu_elapsed)]);
iflag_main = 1;
return;


% =========================================================
% This routine calculates the function values for the
% set of equations that sovle for the dimensionless
% concentration profile within the catalyst pellet
% for a nonisothermal first order reaction.
% K. Beers. MIT ChE. 9/21/2002
function [fval,Jac] = catalyst_nonisothermal_calc_f(phiA, ...
    Phi_TM, beta, gamma, z_grid);

num_grid = length(z_grid);

% allocate space for function vector
% and Jacobian
fval = zeros(size(phiA));
Jac = spalloc(num_grid,num_grid,3*num_grid);

% For each point except the first and last, compute
% the value of the function.
for j=2:(num_grid-1)
    
    % estimate the local second derivative in
    % spherical coordinates
    z_mid_up = (z_grid(j+1)+z_grid(j))/2;
    z_mid_lo = (z_grid(j)+z_grid(j-1))/2;
    dz_mid = z_mid_up - z_mid_lo;
    dz_up = z_grid(j+1) - z_grid(j);
    dz_lo = z_grid(j) - z_grid(j-1);

    A_lo = (z_mid_lo^2)/(dz_lo*dz_mid);
    A_mid = (-1/dz_mid)*((z_mid_up^2)/dz_up + ...
                         (z_mid_lo^2)/dz_lo);
    A_up = (z_mid_up^2)/(dz_up*dz_mid);
    
    second_deriv = A_lo*phiA(j-1) + A_mid*phiA(j) + ...
        A_up*phiA(j+1);
    
    % compute nonlinear reaction term
    theta = exp(  gamma*beta*(1-phiA(j)) / ...
                    (1 + beta*(1-phiA(j))));
    rxn = (z_grid(j)^2)*(Phi_TM^2)*theta*phiA(j);
    
    % compute function value
    fval(j) = second_deriv - rxn;
    
    % compute Jacobian elements
    Jac(j,j-1) = A_lo;
    Jac(j,j+1) = A_up;
    dtheta_dphiA = theta* ( ...
        (-gamma*beta)/(1+beta*(1-phiA(j))) + ...
        (gamma*beta^2*(1-phiA(j)))/((1+beta*(1-phiA(j)))^2));
    drxn_dphiA = (z_grid(j)^2)*(Phi_TM^2) * ...
        (theta + phiA(j)*dtheta_dphiA);
    Jac(j,j) = A_mid - drxn_dphiA;    
end

% Now, calculate function value for last point,
% employing known concentration at the catalyst
% surface.
z_mid_up = (1+z_grid(num_grid))/2;
z_mid_lo = (z_grid(num_grid) + ...
    z_grid(num_grid-1))/2;
dz_mid = z_mid_up - z_mid_lo;
dz_up = 1 - z_grid(num_grid);
dz_lo = z_grid(num_grid) - z_grid(num_grid-1);
A_lo = (z_mid_lo^2)/(dz_lo*dz_mid);
A_mid = (-1/dz_mid)*((z_mid_up^2)/dz_up + ...
                     (z_mid_lo^2)/dz_lo);
A_up = (z_mid_up^2)/(dz_up*dz_mid);    
second_deriv = A_lo*phiA(num_grid-1) + ...
    A_mid*phiA(num_grid) + A_up;    
% compute nonlinear reaction term
theta = exp(  gamma*beta*(1-phiA(num_grid)) / ...
                (1 + beta*(1-phiA(num_grid))));
rxn = (z_grid(num_grid)^2)*(Phi_TM^2)*theta*phiA(num_grid);    
% compute function value
fval(num_grid) = second_deriv - rxn;
% compute Jacobian elements
j = num_grid;
Jac(j,j-1) = A_lo;
dtheta_dphiA = theta* ( ...
    (-gamma*beta)/(1+beta*(1-phiA(j))) + ...
    (gamma*beta^2*(1-phiA(j)))/((1+beta*(1-phiA(j)))^2));
drxn_dphiA = (z_grid(j)^2)*(Phi_TM^2) * ...
    (theta + phiA(j)*dtheta_dphiA);
Jac(j,j) = A_mid - drxn_dphiA;

% Then, calculate function value for
% first point, using symmetry boundary
% condition.
z_mid_up = (z_grid(2)+z_grid(1))/2;
z_mid_lo = (z_grid(1))/2;
dz_mid = z_mid_up - z_mid_lo;
dz_up = z_grid(2) - z_grid(1);
dz_lo = z_grid(1);
A_lo = (z_mid_lo^2)/(dz_lo*dz_mid);
A_mid = (-1/dz_mid)*((z_mid_up^2)/dz_up + ...
                     (z_mid_lo^2)/dz_lo);
A_up = (z_mid_up^2)/(dz_up*dz_mid);    
second_deriv = (A_mid+4*A_lo/3)*phiA(1) + ...
    (A_up - A_lo/3)*phiA(2);
% compute nonlinear reaction term
theta = exp(  gamma*beta*(1-phiA(1)) / ...
                (1 + beta*(1-phiA(1))));
rxn = (z_grid(1)^2)*(Phi_TM^2)*theta*psi(1);    
% compute function value
fval(1) = second_deriv - rxn;
% compute Jacobian elements
j = 1;
Jac(j,j+1) = A_up - A_lo/3;
dtheta_dphiA = theta* ( ...
    (-gamma*beta)/(1+beta*(1-psi(j))) + ...
    (gamma*beta^2*(1-phiA(j)))/((1+beta*(1-phiA(j)))^2));
drxn_dphiA = (z_grid(j)^2)*(Phi_TM^2) * ...
    (theta + phiA(j)*dtheta_dphiA);
Jac(j,j) = A_mid + 4*A_lo/3; - drxn_dphiA;

return;


% =======================================================
% This routine computes the effectiveness factor from
% the dimensionless concentration profile for a
% catalyst particle with a nonisothermal first order
% reaction.
% K. Beers. MIT ChE. 9/21/2002
function eta_eff = calc_eta_eff(phiA, ...
        Phi_TM, beta, gamma, z_grid);

% compute the augmented z grid including the end
% points
z_aug = [0, z_grid, 1];
phiA_aug = [0, phiA, 1];
phiA_aug(1) = (4*phiA(1)-phiA(2))/3;

% Now, compute on this augmented grid the values
% of the integrand.
integrand = zeros(size(phiA_aug));
for j=1:length(integrand)
    var1 = gamma*beta*(1-phiA_aug(j)) / ...
        (1 + beta*(1-phiA_aug(j)));
    integrand(j) = 3*phiA_aug(j)*(z_aug(j)^2)*exp(var1);
end

% Use the trapezoid rule to compute the integral.
eta_eff = trapz(z_aug,integrand);

return;
