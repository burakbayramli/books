function res=bfl(Y,ta,d,s);
% PURPOSE: Temporal disaggregation using the Boot-Feibes-Lisman method
% -----------------------------------------------------------------------
% SYNTAX: res=bfl(Y,ta,d,s);
% -----------------------------------------------------------------------
% OUTPUT: res: a structure
%         res.meth  = 'Boot-Feibes-Lisman';
%         res.N     = Number of low frequency data
%         res.ta    = Type of disaggregation
%         res.d     = Degree of differencing
%         res.s     = Frequency conversion
%         res.y     = High frequency estimate
%         res.et    = Elapsed time
% -----------------------------------------------------------------------
% INPUT: Y: Nx1 ---> vector of low frequency data
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
% LIBRARY: sw
% -----------------------------------------------------------------------
% SEE ALSO: sw, tduni_print, tduni_plot
% -----------------------------------------------------------------------
% REFERENCE: Boot, J.C.G., Feibes, W. and Lisman, J.H.C. (1967)
% "Further methods of derivation of quarterly figures from annual data",
% Applied Statistics, vol. 16, n. 1, p. 65-75.

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

t0=clock;

% -----------------------------------------------------------------------
% Size of the problem

[N,M] = size(Y);
n=s*N;

% -----------------------------------------------------------------------
% Generation of VCV matrix of high-frequency stationary series

v = eye(n-d);

% -----------------------------------------------------------------------
% Calling Stram-Wei procedure under hypothesis y~I(d)

rex = sw(Y,ta,d,s,v);
y = rex.y; clear rex;

% -----------------------------------------------------------------------
% Loading the structure
% -----------------------------------------------------------------------

% Basic parameters

res.meth = 'Boot-Feibes-Lisman';
res.N = N;
res.ta= ta;
res.s = s;
res.d = d;

% -----------------------------------------------------------------------
% Series

res.y = y;

% -----------------------------------------------------------------------
% Elapsed time

res.et        = etime(clock,t0);
