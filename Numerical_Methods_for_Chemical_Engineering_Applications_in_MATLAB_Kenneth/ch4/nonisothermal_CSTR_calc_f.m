% nonisothermal_CSTR_calc_f.m
% This function calculates the two function values that
% are the time derivatives for a non-isothermal CSTR with
% the reaction A --> B.
% K.J. Beers. MIT ChE. 9/24/03
function f = nonisothermal_CSTR_calc_f(x,Param);

% extract the unknowns and parameters to natural names
phi_A = x(1);  % dimensionless A concentration
theta = x(2);  % dimensionless temperature
Da = 10^Param(1);  % Damkoehler number
beta = Param(2);  % dimensionless heat of reaction
chi = Param(3);  % dimensionless heat transfer constant
gamma = Param(4);  % dimensionless activation energy
theta_c = Param(5);  % dimensionless coolant temperature

% Now, we compute the function values
rate_ratio = exp( gamma*(theta-1)/theta );
f = zeros(2,1);
f(1) = 1 - phi_A - Da*rate_ratio*phi_A;
f(2) = 1 - theta - beta*Da*rate_ratio*phi_A - chi*(theta-theta_c);

return;
