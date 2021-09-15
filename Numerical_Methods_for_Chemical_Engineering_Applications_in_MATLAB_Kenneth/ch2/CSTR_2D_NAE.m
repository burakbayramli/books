% CSTR_2D_NAE.m
% 2-D example of CSTR calculation with homotopy
% K. Beers. MIT ChE. 10.34. 7/14/03

function iflag_main = CSTR_2D_NAE();

iflag_main = 0;

% First, set values of system parameters
Param.Q = 0.1;  Param.V = 1;
Param.A_in = 1;  Param.B_in = 2;
Param.C_in = 0;  Param.D_in = 0;
Param.k1 = 1;  Param.k2 = 1;
% Ask user whether to use homotopy
iuse_homotopy = input('Use homotopy 0=no, 1=yes : ');
% set options for fsolve() MATLAB solver
Options = optimset('LargeScale', 'off');
% make initial guess using inlet conc.
x0 = [Param.A_in; Param.B_in; Param.C_in; Param.D_in];

if(~iuse_homotopy)  % solve without homotopy
    [x,fval,exitflag] = fsolve(@CSTR_2D_NAE_calc_f,x0,Options,Param);
        
else  % use homotopy
    Q_set = Param.Q;  Q_max = Param.Q * 1000;  num_homotopy = 5;
    Q_vect = logspace(log10(Q_max),log10(Q_set),num_homotopy);
    for iter=1:num_homotopy
        Param.Q = Q_vect(iter);
        [x,fval,exitflag] = fsolve(@CSTR_2D_NAE_calc_f,x0,Options,Param);
        if(exitflag<=0)
            disp(['Not coverged at iter = ', int2str(iter)]);
            return;
        end
        x0 = x;  % use last result as next initial guess
    end
end

% display result
if(exitflag==0)
    disp('Max. # of iterations reached without convergence');
    return;
elseif(exitflag<0)
    disp(['Error with exitflag = ', int2str(exitflag)]);
    return;
else
    disp('Converged to a solution');  disp(' ');
    disp('The steady state concentrations are : ');
    disp(['[A] = ', num2str(x(1))]);
    disp(['[B] = ', num2str(x(2))]);
    disp(['[C] = ', num2str(x(3))]);
    disp(['[D] = ', num2str(x(4))]);
end

iflag_main = 1;
return;


%=======================
function f = CSTR_2D_NAE_calc_f(x,Param);

f = zeros(4,1);
f(1) = Param.Q*(Param.A_in - x(1)) + ...
    Param.V*(-Param.k1*x(1)*x(2));
f(2) = Param.Q*(Param.B_in - x(2)) + ...
    Param.V*(-Param.k1*x(1)*x(2) - Param.k2*x(3)*x(2));
f(3) = Param.Q*(Param.C_in - x(3)) + ...
    Param.V*(Param.k1*x(1)*x(2) - Param.k2*x(3)*x(2));
f(4) = Param.Q*(Param.D_in - x(4)) + ...
    Param.V*(Param.k2*x(3)*x(2));

return;
