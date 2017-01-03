function grdd = gradt(funfcn,x0,varargin)
% PURPOSE: computes gradient vector
%          for the likelihood function f evaluated at b
%---------------------------------------------------
% USAGE:gradd = gradt(f,b,y,x,varargin)
% where:    f = a string containing the likelihood function
%           b = a parameter vector (k x 1)
%           y = data vector
%           x = data matrix
%    varargin = arguments list passed to func
%---------------------------------------------------       
% NOTE: f must take the form f(b,y,x,args)
%       where: b = parameter vector (k x 1)
%           args = any # of arguments passed to the function
%---------------------------------------------------       
% RETURNS: gradd = gradient vector (1 x k)      
%---------------------------------------------------       
% SEE ALSO: updateh, hessian
%---------------------------------------------------       

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

funfcn = fcnchk(funfcn,length(varargin));

[k junk] = size(x0);
if k == 1
x0 = x0';
end;

% NOTE: we assume an nobs vector or matrix here
%       pretty shoddy practice really
[n junk] = size(varargin{1});

      
    xarg = x0;
    f0 = feval(funfcn,x0,varargin{:});
    gradd = zeros(n,k);
    epsilon = sqrt(eps);
    iota = ones(k,1);

    absx0 = abs(x0);
    tst0 = find(absx0 == 0);
    absx0(tst0) = 0.00001*ones(length(tst0),1);
       
    if x0 == 0;
       dabsx0 = 1;
    else
        dabsx0 = x0./absx0;
    end;
    dh = epsilon*max(([absx0; (1e-2)*iota])').*dabsx0;
    xdh = x0+dh;
    dh = xdh-x0;  
    
    tst0 = find(dh == 0);
    dh(tst0) = 0.00001*ones(length(tst0),1);

    tmp = zeros(k,k);
    for i=1:k;
    tmp(:,i) = x0;
    end;
      
    for i=1:k;
    tmp(i,i) = xdh(i,1);
    end;
    
    for i = 1:k;
       xarg = tmp(:,i);
       gradd(:,i) = feval(funfcn,xarg,varargin{:});
    end;

    tmp2 = zeros(n,k);
    for i=1:k;
    tmp2(:,i) = gradd(:,i) - f0;
    end;

    gradd = zeros(n,k);
    dhp = dh';
    for i=1:n;
    gradd(i,:) = tmp2(i,:)./dhp;
    end;   
    
    grdd = sum(gradd);
    
