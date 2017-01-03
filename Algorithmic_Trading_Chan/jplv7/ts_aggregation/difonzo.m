function res = difonzo(Y,x,z,ta,s,type,f);
% PURPOSE: Multivariate temporal disaggregation with transversal constraint
% ----------------------------------------------------------------------------------
% SYNTAX: res = difonzo(Y,x,z,ta,s,type,f);
% ----------------------------------------------------------------------------------
% OUTPUT: res: a structure
%         res.meth  = 'Multivariate di Fonzo';
%         res.N     = Number of low frequency data
%         res.n     = Number of high frequency data
%         res.pred  = Number of extrapolations
%         res.ta    = Type of disaggregation
%         res.s     = Frequency conversion
%         res.type  = Model for high frequency innovations
%         res.beta  = Model parameters 
%         res.y     = High frequency estimate
%         res.d_y   = High frequency estimate: std. deviation
%         res.et    = Elapsed time
% ----------------------------------------------------------------------------------
% INPUT: Y: NxM  ---> M series of low frequency data with N observations
%        x: nxm  ---> m series of high frequency data with n observations, m>=M see (*)
%        z: nzx1 ---> high frequency transversal constraint with nz obs.
%        ta: type of disaggregation
%            ta=1 ---> sum (flow)
%            ta=2 ---> average (index)
%            ta=3 ---> last element (stock) ---> interpolation
%            ta=4 ---> first element (stock) ---> interpolation
%        s: number of high frequency data points for each low frequency data points 
%            s= 4 ---> annual to quarterly
%            s=12 ---> annual to monthly
%            s= 3 ---> quarterly to monthly
%        type: model for the high frequency innvations
%            type=0 ---> multivariate white noise
%            type=1 ---> multivariate random walk
% (*) Optional:
%        f: 1xM ---> Set the number of high frequency indicators linked to
%                    each low frequency variable. If f is explicitly included,
%                    the high frequency indicators should be placed in 
%                    consecutive columns
% ----------------------------------------------------------------------------------
% NOTE: Extrapolation is automatically performed when n>sN. 
%       If n=nz>sN restricted extrapolation is applied.
%       Finally, if n>nz>sN extrapolation is perfomed in constrained
%       form in the first nz-sN observatons and in free form in 
%       the last n-nz observations.
% ----------------------------------------------------------------------------------
% LIBRARY: aggreg, dif, vec, desvec
% ----------------------------------------------------------------------------------
% SEE ALSO: denton, mtd_print, mtd_plot
% ----------------------------------------------------------------------------------
% REFERENCE: Di Fonzo, T.(1990)"The estimation of M disaggregate time 
% series when contemporaneous and temporal aggregates are known", Review 
% of Economics and Statistics, vol. 72, n. 1, p. 178-182.

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)
% ----------------------------------------------------------------------------------

t0 = clock;

%--------------------------------------------------------
%       Preliminary checking

[N,M] = size(Y);
[n,m] = size(x);
[nz,mz] = size(z);

if ((M > m) | (n < s*N) | (mz ~= 1) | (nz > n) | (nz < s*N))
   error (' *** INCORRECT DIMENSIONS *** ');
else
   % Number of extrapolations
   h1 = n - nz;
   h2 = n - s*N;
   clear nz mz;
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

%--------------------------------------------------------
%       Checking of "type"

if (type < 0) | (type > 1)
    error (' *** INCORRECT TYPE OPTION *** ');
end

%--------------------------------------------------------
%       Checking (and definition) of vector f 

if (nargin == 6)
    f = ones(1,M);
end

if ( (f < 1) | (sum(f) ~= m) | (length(f) ~= M))
   error (' *** IMPROPER ASSIGNMENT OF INDICATORS IN f VECTOR *** ');
end

%--------------------------------------------------------
%  **** CONSTRAINT MATRICES ***
%--------------------------------------------------------
% Required:
%              H1 ---> transversal
%              H2 ---> longitudinal
%
%---------------------------------------------------------------
%       Generate H1: (n-h1) x nM

H1 = kron(ones(1,M),[eye(n-h1) zeros(n-h1,h1)]);

%---------------------------------------------------------------
%       Generate H2: NM x nM.
%
% Generation of aggregation matrix C

C = aggreg(ta,N,s);
C = [C zeros(N,h2)];

H2 = kron(eye(M),C);

%---------------------------------------------------------------
%       Generate H: (n-h1+NM) x nM.
%
%       H = [ H1
%             H2 ]

H = [H1
   H2];

%--------------------------------------------------------
%  **** PREPARING DATA MATRICES ***
%--------------------------------------------------------
% Required:
%               x_diag
%               Y_big,  Y_e
%               X_diag, X_e

%--------------------------------------------------------
%       Generate x_diag: nM x M+m
%
% It is a diagonal matrix formed by the high frequency
% indicators, including a vector of ones for the intercept
%
%       x_diag = [ x1 0  0  ... 0
%                  0  x2 0  ... 0
%                  0  0  x3 ... 0
%                  ..............
%                  0  0  0  ... xM ]
%
% It is made by means of a recursion.

ac(1)=f(1);                             % Initialization of the recursion
x_diag = [ones(n,1) x(:,1:ac(1))];

j=2;
while (j <= M)
   xaux = [ones(n,1) x(:,ac(j-1)+1:ac(j-1)+f(j))];
   [a2,b2] = size(xaux);
   [a1,b1] = size(x_diag);
   x_diag = [ x_diag         zeros(a1,b2) 
              zeros(a2,b1)   xaux ];
   ac(j) = ac(j-1) + f(j);
   j = j + 1;
end
clear xaux;

%--------------------------------------------------------
%       Generate X_diag: NM x M+m
%
% Low frequency analog of x_diag. It is the result of
% applying the temporal aggregator H2 to x_diag.

X_diag = H2 * x_diag;

%--------------------------------------------------------
%       Generate X_e: (n-h1+NM) x M+m
%
%
% It is the result of applying the complete aggregator H
% (temporal as well as transversal). 
% Lower part of X_e is X_diag.

X_e = H * x_diag;

%--------------------------------------------------------
%       Generate Y_big: NM x 1
%
% It is column vector containing all the observations on the
% low frequency series according to: Y_big = [Y1 Y2 ... YM]'
% Formally: Y_big = vec(Y)

Y_big = vec(Y);

%--------------------------------------------------------
%       Generate Y_e: (n-h1+NM) x 1
%
% It is column vector containing the transversal constraint
% and all the observations on the low frequency series
% according to: Y_e = [ z Y1 Y2 ... YM]' = [z Y_big]'

Y_e = [ z
      Y_big];

%--------------------------------------------------------
%  **** PRELIMINARY ESTIMATION OF SIGMA ***
%--------------------------------------------------------
% The method of di Fonzo requires the previous estimation of VCV
% matrix SIGMA for the (implied) low frequency model. This
% preliminary estimation is performed by means of estimating, equation 
% by equation, the model. Formally, this is equivalent to estimate an 
% unrelated SURE model. Computationally, this is also the applied procedure.

BETA = (X_diag' * X_diag) \ (X_diag' * Y_big); % OLS estimator
U_big = Y_big - X_diag * BETA;                 % Residuals in vec format

% Residuals (columnwise) U: NxM

U = desvec(U_big,M);

% Preliminary estimation of SIGMA

SIGMA = cov(U,1);

%--------------------------------------------------------
%  **** APPLYING DI FONZO PROCEDURE ***
%--------------------------------------------------------

%--------------------------------------------------------
%       High frequency VCV matrix v: nM x nM

switch type
case 0 % White noise
   v = kron(SIGMA,eye(n));
case 1 % Random walk, with U(0)=0
   D = dif(1,n);
   DDi = inv(D'*D);
   v = kron(SIGMA,DDi);
end;

%--------------------------------------------------------
%       Low frequency VCV matrix V: (n-h1+NM) x (n-h1+NM)
%       and its generalized inverse 

V = H * v * H';
Vi = pinv(V);      % Moore-Penrose generalized inverse 

%--------------------------------------------------------
%       Generation of distribution filter L: nM x (n-h1+NM)

L = v * H' * Vi;

%--------------------------------------------------------
%	     GLS estimation of beta in a SURE context

beta = (X_e' * Vi * X_e) \ (X_e' * Vi * Y_e);

U_e = Y_e - X_e * beta;

%--------------------------------------------------------
%       Estimation of high frequency series

y_big = x_diag * beta + L * U_e;

% Series y columnwise y: nxM

y = desvec(y_big,M);

%--------------------------------------------------------
%       VCV matrix of estimations y: nM x nM

sigma_y = (eye(n*M) - L*H)*v + ...
   (x_diag - L*X_e)*inv(X_e'*Vi*X_e)*(x_diag - L*X_e)';

% Vector format of std. dev.

d_y_big = sqrt(diag(sigma_y));

% Std. dev. series in column format dt_y: n x M

d_y = desvec(d_y_big,M);

% -----------------------------------------------------------------------
% Loading the structure
% -----------------------------------------------------------------------
% Basic parameters 

res.meth = 'Multivariate di Fonzo';
res.N = N;
res.n = n;
res.pred = h2;
res.ta= ta;
res.s = s;
res.type = type;

% -----------------------------------------------------------------------
% Parameters

res.beta=beta;

% -----------------------------------------------------------------------
% Series

res.y   = y;
res.d_y = d_y;

% -----------------------------------------------------------------------
% Elapsed time

res.et        = etime(clock,t0);
