function res=denton_uni(Y,x,ta,d,s);
% PURPOSE: Temporal disaggregation using the Denton method
% -----------------------------------------------------------------------
% SYNTAX: res=denton_uni(Y,x,ta,d,s);
% -----------------------------------------------------------------------
% OUTPUT: res: a structure
%         res.meth  = 'Denton';
%         res.N     = Number of low frequency data
%         res.ta    = Type of disaggregation
%         res.s     = Frequency conversion
%         res.d     = Degree of differencing 
%         res.y     = High frequency estimate
%         res.x     = High frequency indicator
%         res.U     = Low frequency residuals
%         res.u     = High frequency residuals
%         res.et    = Elapsed time
% -----------------------------------------------------------------------
% INPUT: Y: Nx1 ---> vector of low frequency data
%        x: nx1 ---> vector of low frequency data
%        ta: type of disaggregation
%            ta=1 ---> sum (flow)
%            ta=2 ---> average (index)
%            ta=3 ---> last element (stock) ---> interpolation
%            ta=4 ---> first element (stock) ---> interpolation
%        d: objective function to be minimized: volatility of ...
%            d=0 ---> levels
%            d=1 ---> first differences
%            d=2 ---> second differences
%        s: number of high frequency data points for each low frequency data point 
%            s= 4 ---> annual to quarterly
%            s=12 ---> annual to monthly
%            s= 3 ---> quarterly to monthly
% -----------------------------------------------------------------------
% LIBRARY: aggreg, bfl
% -----------------------------------------------------------------------
% SEE ALSO: tduni_plot, tduni_print
% -----------------------------------------------------------------------
% REFERENCE: Denton, F.T. (1971) "Adjustment of monthly or quarterly 
% series to annual totals: an approach based on quadratic minimization", 
% Journal of the American Statistical Society, vol. 66, n. 333, p. 99-102.

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

t0=clock;

% -----------------------------------------------------------------------
% Size of the problem

[N,M] = size(Y);
[n,m] = size(x);

if ((n ~= s*N) | (m > 1) | (M > 1))
    error ('*** INCORRECT DIMENSIONS ***');
else
    clear m M;
end

% -----------------------------------------------------------------------
% Generation of aggregation matrix C

C = aggreg(ta,N,s);

% -----------------------------------------------------------------------
% Temporal aggregation matrix of the indicator

X = C*x;

% -----------------------------------------------------------------------
% Computation of low frequency residuals

U = Y - X;

% -----------------------------------------------------------------------
% Computation of high frequency series = indicator + annual residuals
% temporally disaggregated by means of Boot-Feibes-Lisman

res_bfl=bfl(U,ta,d,s);
u=res_bfl.y;

y = x + u;

% -----------------------------------------------------------------------
% Loading the structure
% -----------------------------------------------------------------------

% Basic parameters 

res.meth = 'Denton';
res.N = N;
res.ta= ta;
res.s = s;
res.d = d;

% -----------------------------------------------------------------------
% Series

res.y = y;
res.x = x;
res.U = U;
res.u = u;

% -----------------------------------------------------------------------
% Elapsed time

res.et        = etime(clock,t0);
