function [x,y] = bsp04g(bs,s)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
%   backfacing step, Geometriedaten
%   NE=bsp03g gives the number of boundary segment
%   D=bsp03g(bs) gives a matrix with one column for each
%   boundary segment specified in BS.
%   Row 1 contains the start parameter value.
%   Row 2 contains the end parameter value.
%   Row 3 contains the number of the left hand region.
%   Row 4 contains the number of the right hand region.
%   [X,Y]=bsp03g(BS,S) gives coordinates of boundary points.
%   BS specifies the boundary segments and S the
%   corresponding parameter values. BS may be a scalar.

% -- number of boundary segments ---------
nbs=45;
if nargin == 0,x = nbs; return, end

% d = [start par. value; end par. value;
%       left hand region; right hand region]

d1 = [...
% 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
  0 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0;
  1 1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1;
  1 1 1 1 2 2 2 3 3 3  4  4  4  5  5  6  6  6  7  7  8  8  8  9  9;
  3 4 0 2 0 0 0 0 0 5  5  6  0  0  7  7  8  0  0  9  9  10  0  0  11];
d2 = [...
% 26  27  28  29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45
   0   0   0   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0;
   1   1   1   1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1;
  10  10  10  11 11  12 12 12 13 13 14 14 14 15 15 16 16 16 17 17;
  11  12   0   0 13  13 14  0  0 15 15 16  0  0 17 17 0  0  0  0];
d = [d1,d2];
% -- Randsegmente  ----------------
bs1 = bs(:)';
if find(bs1<1 | bs1>nbs),
  error('Non existent boundary segment number'), end

if nargin == 1, x = d(:,bs1); return, end
x = zeros(size(s)); y = zeros(size(s));
[m,n] = size(bs);
if m == 1 & n == 1,
   bs = bs*ones(size(s)); % expand bs
elseif m ~= size(s,1) | n ~= size(s,2),
   error('bs must be scalar or of same size as s');
end

if ~isempty(s)
   for K = 1:nbs
      ii = find(bs == K); % boundary segment K
      if length(ii)
        [X,Y,P] = bsp04f(K);
         x(ii) = interp1(P,X,s(ii),'linear');
         y(ii) = interp1(P,Y,s(ii),'linear');
      end
   end
end

function [X,Y,P] = bsp04f(segnr)
% backfacing step, Randsegmente
% A = [X-Werte; Y-Werte];
switch segnr
case 1, A = [0.04,    0.08; 0.02, 0.02];
case 2, A = [0.08,   0.08; 0.02, 0.056];
case 3, A = [0.08,   0.04; 0.056, 0.056];
case 4, A = [0.04, 0.04;  0.056, 0.02];

case 5, A = [0.04,    0; 0.056,  0.056];
case 6, A = [0, 0;   0.056, 0.02];
case 7, A = [0,    0.04;  0.02,  0.02];

case 8, A = [0.04,   0.04; 0.02, 0];
case 9, A = [0.04,   0.08; 0, 0];
case 10, A = [0.08,   0.08; 0, 0.02];

case 11, A = [0.08,   0.12; 0.02, 0.02];
case 12, A = [0.12,   0.12; 0.02, 0.056];
case 13, A = [0.12,   0.08; 0.056, 0.056];
case 14,  A = [0.08,   0.12; 0, 0];
case 15,  A = [0.12,   0.12; 0, 0.02];

case 16, A = [0.12,   0.16; 0.02, 0.02];
case 17, A = [0.16,   0.16; 0.02, 0.056];
case 18, A = [0.16,   0.12; 0.056, 0.056];
case 19,  A = [0.12,   0.16; 0, 0];
case 20,  A = [0.16,   0.16; 0, 0.02];

case 21, A = [0.16,   0.20; 0.02, 0.02];
case 22, A = [0.20,   0.20; 0.02, 0.056];
case 23, A = [0.20,   0.16; 0.056, 0.056];
case 24,  A = [0.16,   0.20; 0, 0];
case 25,  A = [0.20,   0.20; 0, 0.02];

case 26, A = [0.20,   0.24; 0.02, 0.02];
case 27, A = [0.24,   0.24; 0.02, 0.056];
case 28, A = [0.24,   0.20; 0.056, 0.056];
case 29, A = [0.20,   0.24; 0, 0];
case 30, A = [0.24,   0.24; 0, 0.02];

case 31, A = [0.24,   0.28; 0.02, 0.02];
case 32, A = [0.28,   0.28; 0.02, 0.056];
case 33, A = [0.28,   0.24; 0.056, 0.056];
case 34, A = [0.24,   0.28; 0, 0];
case 35, A = [0.28,   0.28; 0, 0.02];

case 36, A = [0.28,   0.32; 0.02, 0.02];
case 37, A = [0.32,   0.32; 0.02, 0.056];
case 38, A = [0.32,   0.28; 0.056, 0.056];
case 39, A = [0.28,   0.32; 0, 0];
case 40, A = [0.32,   0.32; 0, 0.02];

case 41, A = [0.32,   0.36; 0.02, 0.02];
case 42, A = [0.36,   0.36; 0.02, 0.056];
case 43, A = [0.36,   0.32; 0.056, 0.056];
case 44, A = [0.32,   0.36; 0, 0];
case 45, A = [0.36,   0.36; 0, 0.02];


end
N = size(A,2); L = 0; AUX = zeros(1,N); P = AUX;
for I = 1:N-1
    % ungefaehre Laenge der Segmente
    AUX(I) = sqrt((A(1,I+1) - A(1,I))^2  + (A(2,I+1) - A(2,I))^2);
end
L = sum(AUX); AUX = AUX/L;
for I = 2:N, P(I) = P(I-1) + AUX(I-1); end
X = A(1,:); Y = A(2,:);

