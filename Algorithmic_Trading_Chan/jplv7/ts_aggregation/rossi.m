function res = rossi(Y,x,z,ta,s,type);
% PURPOSE: Multivariate temporal disaggregation with transversal constraint
% -----------------------------------------------------------------------------
% SYNTAX: res = rossi(Y,x_ini,z,ta,s,type);
% -----------------------------------------------------------------------------
% OUTPUT: res: a structure
%         res.meth  = 'Multivariate Rossi';
%         res.N     = Number of low frequency data
%         res.n     = Number of high frequency data
%         res.pred  = Number of extrapolations (=0 in this case)
%         res.ta    = Type of disaggregation
%         res.s     = Frequency conversion
%         res.y     = High frequency estimate
%         res.et    = Elapsed time
% -----------------------------------------------------------------------
% INPUT: Y:     NxM ---> M series of low frequency data with N observations
%        x:     nxM ---> M series of high frequency data with n observations
%        z:     nx1 ---> high frequency transversal constraint
%        ta: type of disaggregation
%            ta=1 ---> sum (flow)
%            ta=2 ---> average (index)
%            ta=3 ---> last element (stock) ---> interpolation
%            ta=4 ---> first element (stock) ---> interpolation
%        s: number of high frequency data points for each low frequency data points 
%            s= 4 ---> annual to quarterly
%            s=12 ---> annual to monthly
%            s= 3 ---> quarterly to monthly
%        type: univariate temporal disaggregation procedure used to compute
%        preliminary estimates
%            type = 1 ---> Fernandez
%            type = 2 ---> Chow-Lin 
%            type = 3 ---> Litterman
% -----------------------------------------------------------------------
% LIBRARY: aggreg, vec, desvec, fernandez, chowlin, litterman
% -----------------------------------------------------------------------
% SEE ALSO: denton, difonzo, mtd_print, mtd_plot
% -----------------------------------------------------------------------
% REFERENCE: Rossi, N. (1982)"A note on the estimation of disaggregate 
% time series when the aggregate is known", Review of Economics and Statistics, 
% vol. 64, n. 4, p. 695-696.
% Di Fonzo, T. (1994) "Temporal disaggregation of a system of 
% time series when the aggregate is known: optimal vs. adjustment methods",
% INSEE-Eurostat Workshop on Quarterly National Accounts, Paris, december.

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

t0 = clock;

%--------------------------------------------------------
%       Initial assignment 

x_ini = x; 
clear x;

%--------------------------------------------------------
%       Preliminary checking

[N,M] = size(Y);
[n,m] = size(x_ini);
[nz,mz] = size(z);

if ((M ~=  m) | (n ~= s*N) | (nz ~=  n) | (mz ~=  1) | (nz ~= s*N))
   error (' *** INCORRECT DIMENSIONS *** ');
else
   clear m nz mz;
end

%--------------------------------------------------------
%       Checking of "ta"

if (ta < 1) | (ta > 4)
    error (' *** INCORRECT TA OPTION *** ');
end

%--------------------------------------------------------
%       Checking of "s"

if (s ~= 3) & (s ~= 4) & (s ~= 12)
    error (' *** INCORRECT FREQUENCY CONVERSION (s) *** ');
end

% =========================================================================
% STEP 1: PRELIMINARY ESTIMATES BY MEANS OF UNIVARIATE METHODS
% =========================================================================

x = zeros(n,M); e = zeros(n,M);

for j=1:M
    switch type
        case 1
            rex = fernandez(Y(:,j),x_ini(:,j),ta,s);
        case 2
            rex = chowlin(Y(:,j),x_ini(:,j),ta,s,1); % Max. likelihood estimation
        case 3
            rex = litterman(Y(:,j),x_ini(:,j),ta,s,1); % Max. likelihood estimation
    end
    x(:,j) = rex.y;
    e(:,j) = rex.u;
end

S = cov(e,1);

% =========================================================================
% STEP 2: FINAL ESTIMATES: MULTIVARIATE SOLUTION
% =========================================================================

%--------------------------------------------------------
%  **** CONSTRAINT MATRICES ***
%--------------------------------------------------------
% Required:
%              H1 ---> transversal
%              H2 ---> longitudinal
%
%---------------------------------------------------------------
%       Generate H1: n x nM

H1 = kron(ones(1,M),eye(n));

%---------------------------------------------------------------
%       Generate H2: NM x nM.
%
% Generation of aggregation vector c and matrix C

C = aggreg(ta,N,s);

H2 = kron(eye(M),C);

%---------------------------------------------------------------
%       Generate H: n+NM x nM.
%
%       H = [H1
%          H2 ]

H = [H1
   H2];

%--------------------------------------------------------
%  **** PREPARING DATA MATRICES ***
%--------------------------------------------------------
% Required:
%               x_big
%               Y_big, Y_e
%               X_diag, X_e

%--------------------------------------------------------
%       Generate x_big: nM x 1

x_big = vec(x);

%--------------------------------------------------------
%       Generate Y_big: NM x 1

Y_big = vec(Y);

%--------------------------------------------------------
%       Generate Y_e: n+NM x 1
%
%       	It is column vector containing the transversal
%        constraint and all the observations
%       	on the low frequency series
%  		according to: Y_e = [ z Y1 Y2 ... YM]' = [z Y_big]'

Y_e = [ z
      Y_big];

% -------------------------------------------------------------------------
% FINAL SOLUTION
% -------------------------------------------------------------------------

U_e = Y_e - H * x_big;

W = kron(S,eye(n));
Wi = inv(W);
Vi = pinv (H * Wi * H');

y_big = x_big + Wi * H' * Vi * U_e;

% Series y columnwise y: nxM

y = desvec(y_big,M);

% -----------------------------------------------------------------------
% Loading the structure
% -----------------------------------------------------------------------

% Basic parameters 

res.meth = 'Multivariate Rossi';
res.N = N;
res.n = n;
res.pred = 0;
res.ta= ta;
res.s = s;
res.type = type;

% -----------------------------------------------------------------------
% Series

res.y = y;

% -----------------------------------------------------------------------
% Elapsed time

res.et        = etime(clock,t0);
