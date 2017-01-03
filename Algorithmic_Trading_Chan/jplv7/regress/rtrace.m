function rtrace(y,x,thetamax,divs,vname,flag)
% PURPOSE: Plots ntheta ridge regression estimates 
%---------------------------------------------------
% USAGE: rtrace(y,x,thetamax,ntheta,vnames)
% where: y        = dependent variable vector
%        x        = independent variables matrix
%        thetamax = maximum ridge parameter to try
%        ntheta   = number of values between 0 and thetamax to try
%        vnames   = optional variable names vector
%                   e.g. vnames = strvcat('y,'x1','x2');
%        flag     = 1 to skip intercept parameter (better scaling of plot)
%                   default = 0
%---------------------------------------------------
% RETURNS:
%        nothing, plots the parameter estimates as a function
%        of the ntheta alternative ridge parameter values
% --------------------------------------------------
% SEE ALSO: ridge, theil
%---------------------------------------------------
% REFERENCES: David Birkes, Yadolah Dodge, 1993,
% Alternative Methods of Regression 

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% error checking on inputs
if nargin == 6
    cflag = flag;
    nflag = 1;
elseif nargin == 5
nflag = 1;
cflag = 0;
elseif nargin == 4
nflag = 0;
cflag = 0;
else
error('Wrong # of arguments to rtrace'); 
end;

incr = thetamax/divs;
if cflag == 1;
    [m,n] = size(x);
    vname = strvcat(vname(1,:),vname(3:end,:));

dfs = m - n - 1;
b = zeros(n,divs+1);
[q,r] = qr(x,0); 
xpxi = (r'*r)\eye(n);
b(:,1) = xpxi*(x'*y);
ridi = diag(diag(x'*x));
dif = x*b(:,1)-y;
ssqerr = dif'*dif;
theta = n*(ssqerr/dfs)/sum((ridi.^0.5*b(:,1)).^2);
for i = 1:divs
  b(:,i+1) = inv(x'*x + ridi*i*incr)*(x'*y);
end
b = b(2:n,:);
plot(0:incr:thetamax,b');
 hold on; 
 plot([theta theta],[min(min(b)) max(max(b))],'-g');
hold off; 
title('Values of Regression Coefficients as a Function of \theta');
xlabel('Value of \theta, vertical line shows H-K \theta value');
ylabel('Regression Coefficients');
xcoord = floor(divs/10);
if xcoord == 0
   xcoord = 2;
end

if (nflag == 0)
vnames = [];
 for i=1:n
 vnames{i} = str2mat(['variable ',num2str(i-1)]);
 end;
end;

if (nflag == 1)
[tst_n junk] = size(vname);
if tst_n ~= n
 error('Wrong # of variable names -- check vnames argument in rtrace');
else
 for i=1:n
 vnames{i} = vname(i,:);
 end;
end;

   hx = legend(vnames{2:n});
   set(hx,'Visible','on');
   
end;
    
    
    
elseif cflag == 0;
[m,n] = size(x);

dfs = m - n - 1;
b = zeros(n,divs+1);
[q,r] = qr(x,0); 
xpxi = (r'*r)\eye(n);
b(:,1) = xpxi*(x'*y);
ridi = diag(diag(x'*x));
dif = x*b(:,1)-y;
ssqerr = dif'*dif;
theta = n*(ssqerr/dfs)/sum((ridi.^0.5*b(:,1)).^2);
for i = 1:divs
  b(:,i+1) = inv(x'*x + ridi*i*incr)*(x'*y);
end
plot(0:incr:thetamax,b');
 hold on; 
 plot([theta theta],[min(min(b)) max(max(b))],'-g');
hold off; 
title('Values of Regression Coefficients as a Function of \theta');
xlabel('Value of \theta, vertical line shows H-K \theta value');
ylabel('Regression Coefficients');
xcoord = floor(divs/10);
if xcoord == 0
   xcoord = 2;
end

if (nflag == 0)
vnames = [];
 for i=1:n
 vnames{i} = str2mat(['variable ',num2str(i-1)]);
 end;
end;

if (nflag == 1)
[tst_n junk] = size(vname);
if tst_n ~= n+1
 error('Wrong # of variable names -- check vnames argument in rtrace');
else
 for i=1:n+1
 vnames{i} = vname(i,:);
 end;
end;

   hx = legend(vnames{2:n+1});
   set(hx,'Visible','on');
   
end;

end;
