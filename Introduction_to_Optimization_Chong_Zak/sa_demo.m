function [x,N]=random_search(funcname,xnew,options);
%Simulated annealing demo
% random_search(funcname,xnew,options);
% print = options(1);
% gamma = options(15);
% alpha = options(18);

if nargin ~= 3
  options = [];
  if nargin ~= 2
    disp('Wrong number of arguments.');
    return;
  end
end

if length(options) >= 14 
  if options(14)==0
    options(14)=1000*length(xnew);
  end
else
  options(14)=1000*length(xnew);
end

if length(options) < 15
  options(15)=5.0; %
end
if options(15)==0 
  options(15)=5.0; %
end

if length(options) < 18
  options(18)=0.5; %optional step size
end


%clc;
format compact;
format short e;

options = foptions(options);
print = options(1);
epsilon_x = options(2);
epsilon_g = options(3);
max_iter=options(14);

alpha = options(18);
gamma = options(15);
k0=2;

if funcname == 'f_r',
  ros_cnt
elseif funcname == 'f_p',
  pks_cnt;
end %if

if length(xnew) == 2
  plot(xnew(1),xnew(2),'o')
  text(xnew(1),xnew(2),'Start Point')
  xlower = [-2;-1];
  xupper = [2;3];
end

f_0=feval(funcname,xnew);
xbestcurr = xnew;
xbestold = xnew;
xcurr = xnew;
f_best=feval(funcname,xnew);
f_best=10^(sign(f_best))*f_best;

for k = 1:max_iter,

  f_curr=feval(funcname,xcurr);

  xnew = xcurr + alpha*(2*rand(length(xcurr),1)-1);

  for i=1:length(xnew),
    xnew(i) = max(xnew(i),xlower(i));
    xnew(i) = min(xnew(i),xupper(i));
  end %for

  f_new=feval(funcname,xnew);

  %if abs(f_best-f_new) <= epsilon_g*abs(f_best),
    %disp('Terminating: relative function difference less than');
    %disp(epsilon_g);
    %k=k-1;
      %break;
  %end %if

  if f_new < f_curr,
    xcurr = xnew;
    f_curr = f_new;
  else
    cointoss = rand(1);
    Temp = gamma/log(k+k0);
    Prob = exp(-(f_new-f_curr)/Temp);
    if cointoss < Prob,
      xcurr = xnew;
      f_curr = f_new;
    end
  end

  if f_new < f_best,
    xbestold = xbestcurr;
    xbestcurr = xnew;
    f_best = f_new;
  end

  if print,
    disp('Iteration number k =')
    disp(k);  %print iteration index k
    disp('alpha =');
    disp(alpha);  %print alpha
    disp('New point =');
    disp(xnew'); %print new point
    disp('Function value =');
    disp(f_new); %print func value at new point
  end %if

  if norm(xnew-xbestold) <= epsilon_x*norm(xbestold)
    disp('Terminating: Norm of difference between iterates less than');
    disp(epsilon_x);
    break;
  end %if

  pltpts(xbestcurr,xbestold);

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
  disp(xbestcurr'); 
  disp('Objective function value ='); 
  disp(f_best); 
  disp('Number of iterations ='); 
  disp(k); 
end %if
