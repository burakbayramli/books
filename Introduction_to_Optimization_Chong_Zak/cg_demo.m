function [x,N]=conj_grad(grad,xnew,options);
%Matlab routine for conjugate gradient algorithm 
%using secant method for line search.
%-- by E. K. P. Chong,  Feb. 11, 1994
%
%    CONJ_GRAD('grad',x0);
%    CONJ_GRAD('grad',x0,OPTIONS);
%
%    x = CONJ_GRAD('grad',x0);
%    x = CONJ_GRAD('grad',x0,OPTIONS);
%
%    [x,N] = CONJ_GRAD('grad',x0);
%    [x,N] = CONJ_GRAD('grad',x0,OPTIONS);
%
%The first variant finds the minimizer of a function whose gradient
%is described in grad (usually an M-file: grad.m), using initial point
%x0.
%The second variant allows a vector of optional parameters to be
%defined:
%OPTIONS(1) controls how much display output is given; set 
%to 1 for a tabular display of results, (default is no display: 0). 
%OPTIONS(2) is a measure of the precision required for the final point.
%OPTIONS(3) is a measure of the precision required of the gradient.
%OPTIONS(5) specifies the formula for beta: 
%   0=Powell;
%   1=Fletcher-Reeves; 
%   2=Polak-Ribiere; 
%   3=Hestenes-Stiefel.
%OPTIONS(14) is the maximum number of iterations.
%For more information type HELP FOPTIONS. 
%
%The next two variants return the value of the final point.
%The last two variants return a vector of the final point and the
%number of iterations. 

if nargin ~= 3
  options = [];
  if nargin ~= 2
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

g_curr=feval(grad,xnew);
if norm(g_curr) <= epsilon_g
  disp('Terminating: Norm of initial gradient less than');
  disp(epsilon_g);
  return;
end %if

d=-g_curr;
reset_cnt = 0;
for k = 1:max_iter,

  xcurr=xnew;
  alpha=secant(grad,xcurr,d);
  %alpha=-(d'*g_curr)/(d'*Q*d);
  xnew = xcurr+alpha*d;

  if print,
    disp('Iteration number k =')
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

  reset_cnt = reset_cnt+1;
  if reset_cnt == 3*numvars
    d=-g_curr;
    reset_cnt = 0;
  else
    if options(5)==0 %Powell
      beta = max(0,(g_curr'*(g_curr-g_old))/(g_old'*g_old));
    elseif options(5)==1 %Fletcher-Reeves
      beta = (g_curr'*g_curr)/(g_old'*g_old);
    elseif options(5)==2 %Polak-Ribiere
      beta = (g_curr'*(g_curr-g_old))/(g_old'*g_old);
    else %Hestenes-Stiefel
      beta = (g_curr'*(g_curr-g_old))/(d'*(g_curr-g_old));
    end %if
    d=-g_curr+beta*d;
  end

  if print,
    disp('New beta =');
    disp(beta);
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
