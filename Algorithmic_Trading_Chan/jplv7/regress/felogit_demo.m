% PURPOSE: demonstrate use of felogit.m
% Simon D. Woodcock, sdw9@cornell.edu
% 9/13/2002

%---- GENERATE SOME DEMO DATA WITH A TYPICAL UNBALAMCED ----%
%---- PANEL STRUCTURE                                   ----%

N = 1000;      % number of observational units
T = 20;        % max number of repeated observations
nvar = 5;     % number of covariates
maxit = 10;    % maximum number of iterations
tol = 1e-8;    % convergence tolerance

% unbalance the data by randomly determining the number of repeated
% obseravations for each observational unit
T_i = round(2 + T*rand(N,1));

% create indices for observational units and repetitions
id = round(100000+100000*rand(N,1));      % random numeric id's for obs units
for i = 1:N                               % put the id's and fixed effects in a T x N matrix
    for t = 1:T_i(i,1)
        cmat(t,i) = 5*i/N;                % make the fixed effects a known function of i so we can assess accuracy
        imat(t,i) = id(i,1);
        tmat(t,i) = t;                    % id's for repetitions
    end;
end;

% stack the data id's and fixed effects into conformable vectors
[junk,punk,ivec] = find(imat);
[junk,punk,tvec] = find(tmat);
[junk,punk,c] = find(cmat);
nobs = length(ivec);
clear imat tmat cmat;

% create rest of data
x = randn(nobs,nvar);                     % covariates
beta = ones(nvar,1);                      % slope parameters
e = sqrt(1)*randn(nobs,1);    
P = exp(x*beta + c + e) ./ ( 1 + exp(x*beta + c + e) );
u = rand(nobs,1);
y = u < P;                                % response variable

clear c e P u beta punk junk T_i nvar N T id;
pack;

%---- SHOW WHAT THE DATA LOOKS LIKE ----%
disp('Here is a print of the first 20 observations in our data'); 
look=[ivec(1:20) tvec(1:20) y(1:20) x(1:20,:)]


%---- DEMONSTRATE CALL OF FELOGIT ----%

results = felogit(y,x,ivec,tvec,[],[],maxit,tol);

%---- PRINT RESULTS ----%

prt(results);
