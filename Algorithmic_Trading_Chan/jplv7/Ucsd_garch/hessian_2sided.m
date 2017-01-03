function H = hessian_2sided(f,x,varargin)
% PURPOSE: 
%      Computes 2-sided finite difference Hessian
%
% USAGE:  
%      H = hessian_2sided(func,x,varargin)
%
% INPUTS:
%      func         - function name, fval = func(x,varargin)
%      x            - vector of parameters (n x 1)
%      varargin     - optional arguments passed to the function
%
% OUTPUTS:
%      H            - finite differnce, 2-sided hessian
%
% COMMENTS:
%      Code originally from COMPECON toolbox [www4.ncsu.edu/~pfackler]
%      documentation modified to fit the format of the Ecoometrics Toolbox
%      by James P. LeSage, Dept of Economics
%      University of Toledo
%      2801 W. Bancroft St,
%      Toledo, OH 43606
%      jlesage@spatial-econometrics.com
%
% Further modified (to do 2-sided numerical derivs, rather than 1) by:
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 3    Date: 4/1/2004

try
    feval(f,x,varargin{:});
catch
    error('There was an error evaluating the function.  Please check the arguements.');
end


n = size(x,1);
fx = feval(f,x,varargin{:});

% Compute the stepsize (h)
h = eps.^(1/3)*max(abs(x),1e-2);
xh = x+h;
h = xh-x;    
ee = sparse(1:n,1:n,h,n,n);

% Compute forward and backward steps
gp = zeros(n,1);
for i=1:n
    gp(i) = feval(f,x+ee(:,i),varargin{:});
end
gm = zeros(n,1);
for i=1:n
    gm(i) = feval(f,x-ee(:,i),varargin{:});
end


H=h*h';
Hm=H;
Hp=H;
% Compute "double" forward and backward steps
for i=1:n
    for j=i:n
        Hp(i,j) = feval(f,x+ee(:,i)+ee(:,j),varargin{:});
        Hp(j,i)=Hp(i,j);
        Hm(i,j) = feval(f,x-ee(:,i)-ee(:,j),varargin{:});
        Hm(j,i)=Hm(i,j);        
    end
end

%Compute the hessian
for i=1:n
    for j=i:n
        H(i,j) = (Hp(i,j)-gp(i)-gp(j)+fx+fx-gm(i)-gm(j)+Hm(i,j))/H(i,j)/2;
        H(j,i) = H(i,j);
    end
end
