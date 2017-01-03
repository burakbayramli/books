function res=ssc_fix(Y,x,ta,s,type,phi)
% PURPOSE: Temporal disaggregation using the dynamic Chow-Lin method
%          proposed by Santos Silva-Cardoso (2001). In this function
%          phi parameter is fixed (supplied by the user)
% ------------------------------------------------------------
% SYNTAX: res=ssc_fix(Y,x,ta,s,type,phi);
% ------------------------------------------------------------
% OUTPUT: res: a structure
%         res.meth     ='Santos Silva-Cardoso';
%         res.ta       = type of disaggregation
%         res.type     = method of estimation
%         res.N        = nobs. of low frequency data
%         res.n        = nobs. of high-frequency data
%         res.pred     = number of extrapolations
%         res.s        = frequency conversion between low and high freq.
%         res.p        = number of regressors (+ intercept)
%         res.Y        = low frequency data
%         res.x        = high frequency indicators
%         res.y        = high frequency estimate
%         res.y_dt     = high frequency estimate: standard deviation
%         res.y_lo     = high frequency estimate: sd - sigma
%         res.y_up     = high frequency estimate: sd + sigma
%         res.u        = high frequency residuals
%         res.U        = low frequency residuals
%         res.gamma    = estimated model parameters (including y(0))
%         res.gamma_sd = estimated model parameters: standard deviation
%         res.gamma_t  = estimated model parameters: t ratios
%         res.rho      = dynamic parameter phi
%         res.beta     = estimated model parameters (excluding y(0))
%         res.beta_sd  = estimated model parameters: standard deviation
%         res.beta_t   = estimated model parameters: t ratios
%         res.aic      = Information criterion: AIC
%         res.bic      = Information criterion: BIC
%         res.val      = Objective function used by the estimation method
%         res.r        = grid of dynamic parameters used by the estimation method
%         res.et       = elapsed time
% ------------------------------------------------------------
% INPUT: Y: Nx1 ---> vector of low frequency data
%        x: nxp ---> matrix of high frequency indicators (without intercept)
%        ta: type of disaggregation
%            ta=1 ---> sum (flow)
%            ta=2 ---> average (index)
%            ta=3 ---> last element (stock) ---> interpolation
%            ta=4 ---> first element (stock) ---> interpolation
%        s: number of high frequency data points for each low frequency data points
%            s= 4 ---> annual to quarterly
%            s=12 ---> annual to monthly
%            s= 3 ---> quarterly to monthly
%        type: estimation method:
%            type=0 ---> weighted least squares
%            type=1 ---> maximum likelihood
%        phi: dynamic parameter -1 < phi < 1
% ------------------------------------------------------------
% LIBRARY: aggreg
% ------------------------------------------------------------
% SEE ALSO: ssc, chowlin, litterman, fernandez, td_plot, td_print
% ------------------------------------------------------------
% REFERENCE: Santos, J.M.C. y Cardoso, F.(2001) "The Chow-Lin method
% using dynamic models",Economic Modelling, vol. 18, p. 269-280.

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

t0=clock;

% ------------------------------------------------------------
% Initital check

if ((phi <= -1) | (phi >=1))
   error (' *** DYNAMIC PARAMETER OUTSIDE OF ACCEPTABLE RANGE *** ');
end

% ------------------------------------------------------------
% Size of the problem

[N,M] = size(Y);    % Size of low-frequency input
[n,p] = size(x);    % Size of p high-frequency inputs (without intercept)

% ------------------------------------------------------------
% Preparing the X matrix: including an intercept

e=ones(n,1);
x=[e x];       % Expanding the regressor matrix
p=p+1;         % Number of p high-frequency inputs (+intercept)

% ------------------------------------------------------------
% Generating the aggregation matrix

C = aggreg(ta,N,s);

% -----------------------------------------------------------
% Expanding the aggregation matrix to perform
% extrapolation if needed.

if (n > s * N)
   pred=n-s*N;           % Number of required extrapolations
   C=[C zeros(N,pred)];
else
   pred=0;
end

% -----------------------------------------------------------
% Generation of difference matrix D_phi

I=eye(n); w=I;
LL=zeros(n,n);
for i=2:n
   LL(i,i-1)=-1; % Auxiliary matrix useful to simplify computations
end

D_phi = I + phi*LL;
iD_phi = inv(D_phi);

% -----------------------------------------------------------
% Truncation remainder: q parameter

q=zeros(n,1);
q(1)=phi;

% -----------------------------------------------------------
% Expanded set of regressors: high and low frequency

z = [x q];
z_phi = iD_phi * z;
Z_phi = C * z_phi;

X = C * x;

% -----------------------------------------------------------
% GLS estimator of gamma

w = inv(D_phi' * D_phi);
W = C * w * C';
iW = inv(W);

gamma = (Z_phi' * iW * Z_phi) \ (Z_phi' * iW * Y); % gamma GLS

U = Y - Z_phi * gamma;           % Low frequency residuals
scp = U' * iW * U;               % Weighted least squares
sigma_a = scp/(N-p-1);           % sigma_a estimator (p+1 due to lagged endogenous)
L = w * C' * iW;                 % Filtering matrix
u = L * U;                       % High frequency residuals

% -----------------------------------------------------------
% Temporally disaggregated time series

y = z_phi * gamma + u;

% -----------------------------------------------------------
% Information criteria
% p is expanded to include lagged endogenous

aic=log(sigma_a)+2*(p+1)/N;
bic=log(sigma_a)+log(N)*(p+1)/N;

% -----------------------------------------------------------
% VCV matrix of high frequency estimates

sigma_gamma = sigma_a * inv(Z_phi' * iW * Z_phi);

VCV_y = sigma_a * (eye(n)-L*C) * w + (z_phi-L*Z_phi) * sigma_gamma * (z_phi-L*Z_phi)';

d_y=sqrt((diag(VCV_y)));   % Std. dev. of high frequency estimates
y_li=y-d_y;                % Lower lim. of high frequency estimates
y_ls=y+d_y;                % Upper lim. of high frequency estimates

% -----------------------------------------------------------
% -----------------------------------------------------------
% Loading the structure

res.meth='Santos Silva-Cardoso';

% -----------------------------------------------------------
% Basic parameters

res.ta        = ta;
res.type      = type;
res.N         = N;
res.n         = n;
res.pred      = pred;
res.s         = s;
res.p         = p;

% -----------------------------------------------------------
% Series

res.Y         = Y;
res.x         = x;
res.y         = y;
res.y_dt      = d_y;
res.y_lo      = y_li;
res.y_up      = y_ls;

% -----------------------------------------------------------
% Residuals

res.u         = u;
res.U         = U;

% -----------------------------------------------------------
% Parameters

res.gamma     = gamma;
res.gamma_sd  = sqrt(diag(sigma_gamma));
res.gamma_t   = gamma./sqrt(diag(sigma_gamma));
res.rho       = phi;

res.beta      = gamma(1:end-1);
res.beta_sd   = res.gamma_sd(1:end-1);
res.beta_t    = res.gamma_t(1:end-1);

% -----------------------------------------------------------
% Information criteria

res.aic       = aic;
res.bic       = bic;

% -----------------------------------------------------------
% Objective function (constant)

h_lim=199;        % Number of grid points
inc_phi=0.01;     % Step between grid points
r(1:h_lim)=0;
val(1:h_lim)=0;
r(1)=-0.99;
for h=2:h_lim
   r(h)=r(h-1)+inc_phi;
end

res.val=val;
res.r=r;

% -----------------------------------------------------------
% Elapsed time

res.et        = etime(clock,t0);

