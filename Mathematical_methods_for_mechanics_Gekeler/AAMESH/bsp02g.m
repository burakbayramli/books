function [x,y] = bsp02g(bs,s)
%   flow past half cylinder, Geometriedaten

%   NE=bsp02g gives the number of boundary segment
%
%   D=bsp02g(bs) gives a matrix with one column for each
%   boundary segment specified in BS.
%   Row 1 contains the start parameter value.
%   Row 2 contains the end parameter value.
%   Row 3 contains the number of the left hand region.
%   Row 4 contains the number of the right hand region.
%
%   [X,Y]=bsp02g(BS,S) gives coordinates of boundary points.
%   BS specifies the boundary segments and S the
%   corresponding parameter values. BS may be a scalar.

% -- number of boundary segments ---------
nbs=9;
if nargin == 0,x = nbs; return, end

% d = [start par. value; end par. value;
%       left hand region; right hand region]

d = [...
% 1 2 3 4 5 6 7 8 9
  0 0 0 0 0 0 0 0 0;
  1 1 1 1 1 1 1 1 1;
  1 1 1 1 1 1 2 2 2;
  0 0 0 2 0 0 0 0 0];
% -- Randsegmente  ----------------
bs1 = bs(:)';
if find(bs1<1 | bs1>nbs),
  error('Non existent boundary segment number')
end

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
        [X,Y,P] = bsp02f(K);
         x(ii) = interp1(P,X,s(ii),'linear');
         y(ii) = interp1(P,Y,s(ii),'linear');
      end
   end
end

function [X,Y,P] = bsp02f(segnr)
% flow past half cylinder, Randsegmente
% A = [X-Werte; Y-Werte];
switch segnr
case 1, A = [0  10/3; 0  0];
case 2
   NT = 30;   % hinreichend gross waehlen!
   T  = linspace(pi,0,NT);
   A  = [5 + (5/3)*cos(T); (5/3)*sin(T)];
case 3, A = [20/3  9; 0  0];
case 4, A = [9  9; 0  5];
case 5, A = [9  0; 5  5];
case 6, A = [0  0; 5  0];
case 7, A = [9  20; 0  0];
case 8, A = [20 20; 0  5];
case 9, A = [20  9; 5  5];
end
N = size(A,2); L = 0; AUX = zeros(1,N); P = AUX;
for I = 1:N-1
    % ungefaehre Laenge der Segmente
    AUX(I) = sqrt((A(1,I+1) - A(1,I))^2  + (A(2,I+1) - A(2,I))^2);
end
L = sum(AUX); AUX = AUX/L;
for I = 2:N
    P(I) = P(I-1) + AUX(I-1);
end
X = A(1,:); Y = A(2,:);

