% PURPOSE: An example using box_cox(),
%                           prt(),
%                           plt(),
% Box-Cox regression model
%---------------------------------------------------
% USAGE: box_coxd
%---------------------------------------------------

% generate box-cox model data
n = 100; k = 3; kp1 = k+1;
x = abs(randn(n,k)) + ones(n,k)*10; 
btrue = ones(k,1); epsil = 0.2*randn(n,1);
x = [ones(n,1) x];
y = 10*x(:,1) + x(:,2:k+1)*btrue + epsil;
ycheck = find(y > 0);
if length(ycheck) ~= n
   error('all y-values must be positive');
end;
yt = exp(y); % should produce lambda = 0 estimate
model = 0;   % transform only y-variable
result = boxcox(yt,x,-2,2,model);
prt(result);
model = 1;   % transform both y,x variables
y = 10*x(:,1) + x(:,2:k+1)*btrue + epsil;
ycheck = find(y > 0); xcheck = find(x > 0);
if (length(ycheck) ~= n & length(xcheck) ~= n*kp1)
   error('all y, x-values must be positive');
end;
yt = log(y); % should produce lambda = 1 estimate
xt = log(x); xt(:,1) = ones(n,1);
result = boxcox(yt,xt,-2,2,model);
prt(result);
plt(result);
