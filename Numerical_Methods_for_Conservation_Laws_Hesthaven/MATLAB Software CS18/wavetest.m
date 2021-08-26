function [u] = wavetest(x,a,z,delta,alpha,beta)
% function [u] = wavetest(x,a,z,delta,alpha,beta)
% Purpose: Initial conditions for linear wave test problems.

N = length(x); u = zeros(N,1);
for i=1:N
    xl = x(i);
    % Gaussian wave
    if (-0.8<=xl) & (xl<=-0.6)
        u(i) = 1/6*(Gwave(xl,beta,z-delta) + Gwave(xl,beta,z+delta) + 4*Gwave(xl,beta,z));
    end
    % Square wave
    if (-0.4<=xl) & (xl<=-0.2)
        u(i)=1;
    end
    % Triangle wave
    if (0<=xl) & (xl<=0.2)
      u(i) = 1 - abs(10*(xl-0.1));    
    end
    % Ellipsiodal wave
    if (0.4<=xl) & (xl<=0.6)
      u(i) = 1/6*(Fwave(xl,alpha,a-delta) + Fwave(xl,alpha,a+delta) + 4*Fwave(xl,alpha,a));
    end
end
return
% 
function G = Gwave(x,beta,z)
% function G = Gwave(x,beta,z)
% Purpose: Special function of linear wave equation test
G = exp(-beta*(x-z)^2);
return
%
function F = Fwave(x,alpha,a);
% function F = Fwave(x,alpha,a);
% Purpose: Special function of linear wave equation test
F = sqrt(max(1-alpha^2*(x-a)^2,0));
return
