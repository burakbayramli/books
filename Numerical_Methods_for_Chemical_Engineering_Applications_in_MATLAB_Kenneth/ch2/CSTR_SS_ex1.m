% CSTR_SS_ex1.m
function iflag = CSTR_SS_ex1();
iflag = 0;

% ask the user for the input parameters, which are
% stored in a structure Param
Param.Q = input('Enter vol. flow rate: ');
Param.V = input('Enter reactor volume: ');
Param.k1 = input('Enter rate constant for rxn. # 1: ');
Param.k2 = input('Enter rate constant for rxn. # 2: ');
Param.cA_in = input('Enter inlet A concentration: ');
Param.cB_in = input('Enter inlet B concentration: ');
Param.cC_in = input('Enter inlet C concentration: ');
Param.cD_in = input('Enter inlet D concentration: ');

% set the initial guess equal to the inlet values
x0 = zeros(4,1);
x0(1) = Param.cA_in;  x0(2) = Param.cB_in;
x0(3) = Param.cC_in;  x0(4) = Param.cD_in;

% call fsolve to obtain the steady-state concentrations
Options = optimset('LargeScale','off','Display','off');
[x, f] = fsolve(@CSTR_SS_calc_f, x0, Options, Param);
f_norm = norm(f,inf);  % get infinity norm
% report the results
disp(' '); disp('Steady state concentrations:');
disp(['[A] = ', num2str(x(1))]);
disp(['[B] = ', num2str(x(2))]);
disp(['[C] = ', num2str(x(3))]);
disp(['[D] = ', num2str(x(4))]);
disp(' ');
disp(['infinity norm of f = ', num2str(f_norm)]);

iflag = 1;
return;



%=======================
% This routine returns the function vector for
% the steady-state CSTR example.
function f = CSTR_SS_calc_f(x,Param);

% extract the unknowns into meaningful names
cA = x(1);  cB = x(2);  cC = x(3);  cD = x(4);

% compute the reaction rates
r1 = Param.k1*cA*cB;  r2 = Param.k2*cB*cC;

f = zeros(4,1);
f(1) = Param.Q*(Param.cA_in - cA) + ...
    Param.V*(-r1);
f(2) = Param.Q*(Param.cB_in - cB) + ...
    Param.V*(-r1 - r2);
f(3) = Param.Q*(Param.cC_in - cC) + ...
    Param.V*(r1 - r2);
f(4) = Param.Q*(Param.cD_in - cD) + ...
    Param.V*(r2);

return;