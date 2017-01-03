function [ind,logBayesFactor empMI miss]=condindepEmp(dataX,dataY,dataZ,X,Y,Z,thresh,varargin)
%CONDINDEPEMP Compute the empirical log Bayes Factor and MI for independence/dependence
% [ind,logBayesFactor empMI]=condindepEmp(dataX,dataY,dataZ,X,Y,Z,thresh,<opts>)
% here dataX,dataY,dataZ are data matrices where each row contains the sample states
% with the number of their states in X,Y,Z
% opts.Uxgz, opts.Uygz, opts.Uz, opts.Uxyz are the (scalar) hyperparameters
if isempty(varargin)
    alpha=0.1*size(dataX,2);
    Uxgz=alpha/(prod(X)*prod(Z)); Uygz=alpha/(prod(Y)*prod(Z));
    Uz=alpha/prod(Z); Uxyz=alpha/(prod(X)*prod(Y)*prod(Z));
else
    opts=varargin{1};
    Uxgz=opts.Uxgz; Uygz=opts.Uygz; Uz=opts.Uz; Uxyz=opts.Uxyz;
end

% model of p(x|z)
cxz=count(vertcat(dataX,dataZ),[X Z]); % joint count
logZux = logZdirichlet(Uxgz*ones(prod(X),1));
Cxgz = reshape(cxz,prod(X),prod(Z)); % conditional count
logpxgz = sum(logZdirichlet(Cxgz+Uxgz)-logZux);

% model of p(y|z)
cyz=count(vertcat(dataY,dataZ),[Y Z]);
logZuy = logZdirichlet(Uygz*ones(prod(Y),1));
Cygz = reshape(cyz,prod(Y),prod(Z));
logpygz = sum(logZdirichlet(Cygz+Uygz)-logZuy);

% model of p(z)
cz=count(dataZ,Z);
logZuz = logZdirichlet(Uz*ones(prod(Z),1));
logpz = logZdirichlet(cz+Uz)-logZuz;

logpindep = logpxgz+logpygz+logpz;

% model of p(xyz)
cxyz=count(vertcat(dataX,dataY,dataZ),[X Y Z]);
logZuxyz = logZdirichlet(Uxyz*ones(prod([X Y Z]),1));
logpdep = logZdirichlet(cxyz+Uxyz)-logZuxyz;

logBayesFactor=logpindep-logpdep;

% Empirical Conditional Mutual information:
pxyz=reshape(normp(count(vertcat(dataX,dataY,dataZ),[X Y Z])),[X Y Z]); % empirical distribution

x = 1:length(X);
y = x(end)+1:x(end)+length(Y);
z = y(end)+1:y(end)+length(Z);
emppot.variables=[x y z]; emppot.table=pxyz;
empMI = condMI(emppot,x,y,z);
miss = length(find(cxyz==0)); % number of missing table entries

ind=logBayesFactor>thresh; % use the Bayes Factor to decide