function omega = turbulent_relaxation(nu_lb, nut)
% Compute turbulent vsicosity in lid-driven cavity.
% input variables are 2d matrices.
% nut is the turbulent viscosity that gets added onto the nominal
%   viscosity to produce the total effective viscosity.
% nu_lb: nonminal (scalar) viscosity

tau = 3 * ( nu_lb + nut ) + 0.5;
omega = 1 ./ tau;

