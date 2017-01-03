function  tmean = tsqexact(x,a,b,lambda,mu,sigma)
% FUNCTION TSQEXACT   Compute mean exit time for 
%                     mean-reverting sqrt process.
%                     Use bvp4c to solve ODE numerically.
%
%                     Called by tsqmc
%
%             Input   x is the initial condition: must be between a and b.
%             Output  tmean is the mean exit time.
%

solinit = bvpinit(linspace(a,b,100),@sqinit);
options = bvpset('RelTol',1e-8,'AbsTol',1e-8);
sol = bvp4c(@sq,@sqbc,solinit,options);
tsol = deval(sol,x);    % Evaluates both BVP components at x
tmean = tsol(1);        % We want the first component, y1 

     function yprime = sq(x,y)
     %                          %% Defines the BVP
     ssq = sigma^2;
     f = lambda*(mu-x); 
     g = sigma*sqrt(x);
     hgs = 0.5*g^2;
     yprime = [y(2); (-1-f*y(2))/hgs];
     end

     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     function res = sqbc(ya,yb)
     %                           %% res = 0 defines the bounday conditions
     res = [ya(1); yb(1)];
     end
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     function yinit = sqinit(x)
     %                           %% my initial guess for the BVP solution
     yinit = [sin(pi*(x-1)); pi*cos(pi*(x-1))];
     end

end
