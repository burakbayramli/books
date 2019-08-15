function [x1,v1,fv1] = fminsearcha(funfcn,x0,PHI,Parmeter)
% This is the original MATLAB algorithm,
% slightly modified for the present purposes
% Initialize parameters
rho = 1; chi = 2; psi = 0.5; sigma = 0.5;
n = length(x0);
onesn   = ones(1,n);
two2np1 = 2:n+1;
one2n   = 1:n;
maxiter = Parmeter(1); maxfun = Parmeter(2);
tol = Parmeter(3); %SCHRANKE = Parmeter(4); 
func_evals = 1;
itercount = 0;
how = '';
v = zeros(n,n+1); fv = zeros(1,n+1);
v(:,1) = x0; fv(1) = feval(funfcn,x0); x = x0;
% Following improvement suggested by L.Pfeffer at Stanford
usual_delta = 0.05;             % 5 percent deltas for non-zero terms
zero_term_delta = 0.00025;      % Even smaller delta for zero elements of x
for j = 1:n
    y = x0;
    if y(j) ~= 0
        y(j) = (1 + usual_delta)*y(j);
    else
        y(j) = zero_term_delta;
    end
    v(:,j+1) = y;
    f = feval(funfcn,y);
    fv(1,j+1) = f;
end
% sort so v(1,:) has the lowest function value
[fv,j] = sort(fv);
v = v(:,j);
% Main algorithm
% Iterate until the diameter of the simplex is less than tolx
%   AND the function values differ from the min by less than tolf,
%   or the max function evaluations are exceeded. (Cannot use OR instead of
%   AND.)
DONE = 0;
while ~DONE
   if max(max(abs(v(:,two2np1)-v(:,onesn)))) <= tol && ...
      max(abs(fv(1)-fv(two2np1))) <= tol
      break
   end
   % Compute the reflection point
   % xbar = average of the n (NOT n+1) best points
   xbar = sum(v(:,one2n), 2)/n;
   xr = (1 + rho)*xbar - rho*v(:,end);
   x(:) = xr; fxr = feval(funfcn,x);
   func_evals = func_evals+1;
   %if GRAFIK == 1
   %   plot([v(1,1:3),v(1,1)],[v(2,1:3),v(2,1)],'b'), hold on
   %end
   if fxr < fv(:,1)
      % Calculate the expansion point
      xe = (1 + rho*chi)*xbar - rho*chi*v(:,end);
      x(:) = xe; fxe = feval(funfcn,x);
      func_evals = func_evals+1;
      if fxe < fxr
         v(:,end) = xe;
         fv(:,end) = fxe;
         how = 'expand';
      else
         v(:,end) = xr;
         fv(:,end) = fxr;
         how = 'reflect';
      end
   else % fv(:,1) <= fxr
      if fxr < fv(:,n)
         v(:,end) = xr;
         fv(:,end) = fxr;
         how = 'reflect';
      else % fxr >= fv(:,n)
         % Perform contraction
         if fxr < fv(:,end)
            % Perform an outside contraction
            xc = (1 + psi*rho)*xbar - psi*rho*v(:,end);
            x(:) = xc; fxc = feval(funfcn,x);
            func_evals = func_evals+1;
            if fxc <= fxr
               v(:,end) = xc;
               fv(:,end) = fxc;
               how = 'contract outside';
            else
               % perform a shrink
               how = 'shrink';
            end
         else
            % Perform an inside contraction
            xcc = (1-psi)*xbar + psi*v(:,end);
            x(:) = xcc; fxcc = feval(funfcn,x);
            func_evals = func_evals+1;
            if fxcc < fv(:,end)
               v(:,end) = xcc;
               fv(:,end) = fxcc;
               how = 'contract inside';
            else
               % perform a shrink
               how = 'shrink';
            end
         end
         if strcmp(how,'shrink')
            for j=two2np1
               v(:,j)=v(:,1)+sigma*(v(:,j) - v(:,1));
               x(:) = v(:,j); fv(:,j) = feval(funfcn,x);
            end
            func_evals = func_evals + n;
         end
      end
   end
   [fv,j] = sort(fv);
   v = v(:,j);
   MINWERT_PHI = [fv(1),PHI];
   itercount = itercount + 1;
   DONE = func_evals >= maxfun | itercount >= maxiter | (fv(1) <= PHI);
end
itercount;
x(:) = v(:,1);
fval = min(fv);
x1 = x; v1 = v; fv1 = fv;


