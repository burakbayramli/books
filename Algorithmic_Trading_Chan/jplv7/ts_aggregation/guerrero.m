function res=guerrero(Y,x,ta,s,rexw,rexd);
% PURPOSE: ARIMA-based temporal disaggregation: Guerrero method
% ------------------------------------------------------------
% SYNTAX: res=guerrero(Y,x,ta,s,rexw,rexd);
% ------------------------------------------------------------
% OUTPUT: res: a structure
%         res.meth     ='Guerrero';
%         res.ta       = type of disaggregation
%         res.N        = nobs. of low frequency data
%         res.n        = nobs. of high-frequency data
%         res.pred     = number of extrapolations
%         res.s        = frequency conversion between low and high freq.
%         res.p        = number of regressors (+ intercept)
%         res.Y        = low frequency data
%         res.x        = high frequency indicators
%         res.w        = scaled indicator (preliminary hf estimate)
%         res.y1       = first stage high frequency estimate
%         res.y        = final high frequency estimate
%         res.y_dt     = high frequency estimate: standard deviation
%         res.y_lo     = high frequency estimate: sd - sigma
%         res.y_up     = high frequency estimate: sd + sigma
%         res.delta    = high frequency discrepancy (y1-w)
%         res.u        = high frequency residuals (y-w)
%         res.U        = low frequency residuals (Cu)
%         res.beta     = estimated parameters for scaling x
%         res.k        = statistic to test compatibility
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
%        rexw, rexd ---> a structure containing the parameters of ARIMA model
%            for indicator and discrepancy, respectively (see calT function)
% ------------------------------------------------------------
% LIBRARY: aggreg, calT, numpar, ols
% ------------------------------------------------------------
% SEE ALSO: chowlin, litterman, fernandez, td_print, td_plot
% ------------------------------------------------------------
% REFERENCE: Guerrero, V. (1990) "Temporal disaggregation of time
% series: an ARIMA-based approach", International Statistical
% Review, vol. 58, p. 29-46.

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

t0=clock;

% ------------------------------------------------------------
% Receiving inputs
% ------------------------------------------------------------

[N,M] = size(Y);    % Size of low-frequency input
[n,m] = size(x);    % Size of m high-frequency inputs (without intercept)

if (M > 1) | (s <= 1) | (n < s*N)
   error ('*** INADEQUATE DIMENSIONS OF INPUTS ***');
else
   clear M;
end

% Number of parameters in ARIMA model for w
rw = numpar(rexw);

% Number of parameters in ARIMA model for discrepancy
rd = numpar(rexd);

% ------------------------------------------------------------
% w: Scaling of indicators
% ------------------------------------------------------------

% Preparing the X matrix: including an intercept

e=ones(n,1);
x=[e x];       % Expanding the regressor matrix
p=m+1;         % Number of p high-frequency inputs (+intercept)

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
% Temporal aggregation of the indicators

X=C*x;

% -----------------------------------------------------------
% Derivation of intermediate high-freq. series: w
% w (scaled indicator) is derived by means of OLS applied
% to the low-freq. data Y and X=Cx

%beta = (X'*X) \ (X'*Y);  % OLS are applied to low-freq. data
%beta_t  = NaN * beta;    % Just for output convenience
%beta_sd = NaN * beta;    % Just for output convenience

% Note: if the Econometric Toolbox is not available, change the
% three previous lines by the followig ones:

resbeta = ols(Y,X);  % Calling Econometric Toolbox function ols
beta = resbeta.beta;
beta_t = resbeta.tstat;
beta_sd = beta .* beta_t;

w = x * beta;

% ------------------------------------------------------------
% T: Computing T matrix from phi-weights of ARIMA model of
%    w. Information contained in structure rexw
% ------------------------------------------------------------

T = calT(rexw,s,n);

% -----------------------------------------------------------
% k: Testing the compatibility of Y and W=Cw.
% See Guerrero (1990), p. 38-39.
% Under null of compatibility k ---> chi-square with N-p df
% ------------------------------------------------------------

k = (Y-C*w)'*inv(C*T*T'*C')*(Y-C*w) / rexw.sigma;

% ------------------------------------------------------------
% y1: Preliminary estimation (assuming P=I)
% ------------------------------------------------------------

A1 = (T*T'*C') / (C*T*T'*C');

y1 = w + A1 * (Y - C*w);

% ------------------------------------------------------------
% b: Innovations of the preliminary discrepancy. This series
%    are the basis for testing P=I by means of testing
%    H0: b = white noise
% ------------------------------------------------------------

b = T \ (y1 - w);

% ------------------------------------------------------------
% P: Computing P matrix from phi-weights of ARIMA model of
%    y1-w. Information contained in structure rexd
% ------------------------------------------------------------

Th = calT(rexd,s,n);

if ((rd+rexd.d+rexd.bd) == 0)  % Discrepancy is white noise
   P=eye(n);
else
   P = inv(T) * Th * Th' * inv(T');   % Note: Th*Th'=T*P*T'
end

% ------------------------------------------------------------
% y: Final estimation (including P)
% ------------------------------------------------------------

A = (T*P*T'*C') / (C*T*P*T'*C');

y = w + A * (Y - C*w);

% -----------------------------------------------------------
% Computing sigma: 1x1

u = y - w;
sigma = (u' * inv(T*P*T') * u) / (n-rd);

% -----------------------------------------------------------
% VCV matrix of high frequency estimates

VCV_y = sigma * (eye(n) - A*C) * (T*P*T');

d_y=sqrt(diag(VCV_y));     % Std. dev. of high frequency estimates
y_li=y-d_y;                % Lower lim. of high frequency estimates
y_ls=y+d_y;                % Upper lim. of high frequency estimates

% -----------------------------------------------------------
% Information criteria

aic=log(sigma)+2*p/N;
bic=log(sigma)+log(N)*p/N;

% -----------------------------------------------------------
% Loading the structure
% -----------------------------------------------------------

res.meth='Guerrero';

% -----------------------------------------------------------
% Basic parameters

res.ta        = ta;
res.type      = 2;       % BLUE
res.N         = N;
res.n         = n;
res.pred      = pred;
res.s         = s;
res.p         = p;

% -----------------------------------------------------------
% Series

res.Y         = Y;
res.x         = x;
res.w         = w;
res.y1        = y1;
res.y         = y;
res.y_dt      = d_y;
res.y_lo      = y_li;
res.y_up      = y_ls;

% -----------------------------------------------------------
% ARIMA models

res.rexw = rexw;   % Scaled indicator
res.rexd = rexd;   % Discrepancy

% -----------------------------------------------------------
% Discrepancy & residuals

res.delta     = y1-w;
res.u         = u;
res.U         = C*u;

% -----------------------------------------------------------
% Parameters

res.beta      = beta;
res.beta_sd   = beta_sd;
res.beta_t    = beta_t;
res.rho       = 1.00;

% -----------------------------------------------------------
% Information criteria

res.aic    = aic;  % For output convenience
res.bic    = bic;  % For output convenience
res.k      = k;    % Test of compatibility statistic

% -----------------------------------------------------------
% Elapsed time

res.et        = etime(clock,t0);
