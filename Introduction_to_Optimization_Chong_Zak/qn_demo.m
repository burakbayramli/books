function [x,N]=quasi_newton(grad,xnew,H,options);
%Matlab routine for quasi-Newton algorithms
%using secant method for line search.
%-- by E. K. P. Chong,  Feb. 11, 1994
%
%    QUASI_NEWTON('grad',x0,H0);
%    QUASI_NEWTON('grad',x0,H0,OPTIONS);
%
%    x = QUASI_NEWTON('grad',x0,H0);
%    x = QUASI_NEWTON('grad',x0,H0,OPTIONS);
%
%    [x,N] = QUASI_NEWTON('grad',x0,H0);
%    [x,N] = QUASI_NEWTON('grad',x0,H0,OPTIONS);
%
%The first variant finds the minimizer of a function whose gradient
%is described in grad (usually an M-file: grad.m), using initial point
%x0 and initial inverse Hessian approximation H0.
%The second variant allows a vector of optional parameters to be
%defined:
%OPTIONS(1) controls how much display output is given; set 
%to 1 for a tabular display of results, (default is no display: 0). 
%OPTIONS(2) is a measure of the precision required for the final point.
%OPTIONS(3) is a measure of the precision required of the gradient.
%OPTIONS(5) specifies the formula for the inverse Hessian update: 
%   0=Rank One;
%   1=DFP; 
%   2=BFGS; 
%OPTIONS(14) is the maximum number of iterations.
%For more information type HELP FOPTIONS. 
%
%The next two variants return the value of the final point.
%The last two variants return a vector of the final point and the
%number of iterations. 

if nargin ~= 4
  options = [];
  if nargin ~= 3
    disp('Wrong number of arguments.');
    return;
  end
end

numvars = length(xnew);
if length(options) >= 14 
  if options(14)==0
    options(14)=1000*numvars;
  end
else
  options(14)=1000*numvars;
end

%clc;
format compact;
format short e;

options = foptions(options);
print = options(1);
epsilon_x = options(2);
epsilon_g = options(3);
max_iter=options(14);

ros_cnt;

if length(xnew) == 2
  plot(xnew(1),xnew(2),'o')
  text(xnew(1),xnew(2),'Start Point')
end


reset_cnt = 0;
g_curr=feval(grad,xnew);
if norm(g_curr) <= epsilon_g
  disp('Terminating: Norm of initial gradient less than');
  disp(epsilon_g);
  return;
end %if

d=-H*g_curr;
for k = 1:max_iter,

  xcurr=xnew;
  alpha=secant(grad,xcurr,d);
  xnew = xcurr+alpha*d;

  if print,
    disp('Iteration number k =');
    disp(k);  %print iteration index k
    disp('alpha =');
    disp(alpha);  %print alpha
    disp('Gradient = ');
    disp(g_curr'); %print gradient
    disp('New point =');
    disp(xnew'); %print new point
  end %if

  if norm(xnew-xcurr) <= epsilon_x*norm(xcurr)
    disp('Terminating: Norm of difference between iterates less than');
    disp(epsilon_x);
    break;
  end %if

  g_old=g_curr;
  g_curr=feval(grad,xnew);

  if norm(g_curr) <= epsilon_g
    disp('Terminating: Norm of gradient less than');
    disp(epsilon_g);
    break;
  end %if

  p=alpha*d;
  q=g_curr-g_old;

  reset_cnt = reset_cnt+1;
  if reset_cnt == 3*numvars
    d=-g_curr;
    reset_cnt = 0;
  else
    if options(5)==0 %Rank One
      H = H+(p-H*q)*(p-H*q)'/(q'*(p-H*q));
    elseif options(5)==1 %DFP
      H = H+p*p'/(p'*q)-(H*q)*(H*q)'/(q'*H*q);
    else %BFGS
      H = H+(1+q'*H*q/(q'*p))*p*p'/(p'*q)-(H*q*p'+(H*q*p')')/(q'*p);
    end %if
    d=-H*g_curr;
  end

  if print,
    disp('New H =');
    disp(H);
    disp('New d =');
    disp(d);
  end

  pltpts(xnew,xcurr);

  if k == max_iter
    disp('Terminating with maximum number of iterations');
  end %if
end %for

if nargout >= 1
  x=xnew;
  if nargout == 2
    N=k;
  end 
else
  disp('Final point =');
  disp(xnew');
  disp('Number of iterations =');
  disp(k);
end %if
