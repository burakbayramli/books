function res = sw(Y,ta,d,s,v);
% PURPOSE: Temporal disaggregation using the Stram-Wei method.
% -----------------------------------------------------------------------
% SYNTAX: res = sw(Y,ta,d,s,v);
% -----------------------------------------------------------------------
% OUTPUT: res: a structure
%         res.meth  = 'Stram-Wei';
%         res.N:    = Number of low frequency data
%         res.ta    = Type of disaggregation
%         res.d     = Degree of differencing
%         res.s     = Frequency conversion
%         res.H     = nxN temporal disaggregation matrix
%         res.y     = High frequency estimate
%         res.et    = Elapsed time
% -----------------------------------------------------------------------
% INPUT: Y: Nx1 ---> vector of low frequency data
%        ta: type of disaggregation
%            ta=1 ---> sum (flow)
%            ta=2 ---> average (index)
%            ta=3 ---> last element (stock) ---> interpolation
%            ta=4 ---> first element (stock) ---> interpolation
%        d: number of unit roots
%        s: number of high frequency data points for each low frequency data point
%            s= 4 ---> annual to quarterly
%            s=12 ---> annual to monthly
%            s= 3 ---> quarterly to monthly
%        v: (n-d)x(n-d) VCV matrix of high frequency stationary series
% -----------------------------------------------------------------------
% LIBRARY: aggreg, aggreg_v, dif, movingsum
% -----------------------------------------------------------------------
% SEE ALSO: bfl, tduni_print, tduni_plot
% -----------------------------------------------------------------------
% REFERENCE: Stram, D.O. and Wei, W.W.S. (1986) "A methodological note on the 
% disaggregation of time series totals", Journal of Time Series Analysis, 
% vol. 7, n. 4, p. 293-302. 

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

t0=clock;

%--------------------------------------------------------
%       Preliminary checking

[N,M] = size(Y);

if (M > 1)
    error (' *** INCORRECT DIMENSION OF LOW FREQUENCY INPUT Y *** ');
end

n=s*N; % Number of high frequency observations

[nv,mv] = size(v);

if (nv ~= (n-d)) | (mv ~= (n-d))
    error (' *** INCORRECT DIMENSION OF v MATRIX *** ');
else
    clear nv nm;
end

% ------------------------------------------------------------
%   Computation of H1 matrix
% ------------------------------------------------------------

% ------------------------------------------------------------
% Generation of aggregation vector c

c = aggreg_v(ta,s);

D1=dif(d,n);
D1=D1(d+1:end,:);  % Difference operator without initial conditions

H1=[D1
   zeros(d,n-s*d)  kron(eye(d),c)];

% ------------------------------------------------------------
%   Computation of H2 matrix
% ------------------------------------------------------------

% ------------------------------------------------------------
% Generation of Cd matrix

% Matrix formulation of U(B) filter
% It depends on the type of disaggregation problem at hand

S = movingsum(s,n);  

% Make S square matrix in order to perform SS=S^d
S=[zeros(s-1,n)
   S];

% Computing S^d
% Note that interpolation (ta==3) or (ta==4) requires also the use of the 
% U(B) filter but one time less than in the other cases. The presence
% of the U(B) filter is due to the fact that (1-B)^d applied in the low
% frequency is equivalent to (1-B^s)^d in the high frequency. Since 
% (1-B^s)=(1-B)U(B), the operator needed to induce stationarity in the
% low frequency introduces U(B) in the high frequency.

if ((ta == 3) | (ta == 4))
   switch d
   case 0
      SS = eye(n-d);
   case 1
      SS = S;
   case 2
      SS = S*S;
   end
else 
   switch d
   case 0
      SS=S;
   case 1
      SS=S*S;
   case 2
      SS=S*S*S;
   end
end

% Interpolation matrix. Note that temporal aggregation = 
% = moving temporal aggregation (via U(B)) + systematic sampling.
% Proper care should be taken in the case of average restriction

switch ta
case 1   
    IN = aggreg(3,N,s);      
case 2
    IN = (1/s) * aggreg(3,N,s);      
case 3
    IN = aggreg(3,N,s);      
case 4
    IN = aggreg(3,N,s);      
end

Cd = IN * SS;

Cd = Cd(d+1:end,d+1:end);  % Generation of Cd matrix

D2 = dif(d,N);
D2 = D2(d+1:end,:);  % Difference operator without initial conditions

H2 = [ v * Cd' * inv(Cd * v * Cd') * D2 
    zeros(d,N-d)  eye(d) ];

% ------------------------------------------------------------
%   Computation of H matrix and high-frequency estimate
% ------------------------------------------------------------

H = H1 \ H2;

y = H * Y;

% -----------------------------------------------------------------------
% Loading the structure
% -----------------------------------------------------------------------

% Basic parameters
res.meth = 'Stram-Wei';
res.N = N;
res.ta= ta;
res.s = s;
res.d = d;

% -----------------------------------------------------------------------
% Series and filter
res.H = H;
res.y = y;

% -----------------------------------------------------------------------
% Elapsed time
res.et        = etime(clock,t0);

