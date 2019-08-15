function [x,y] = bsp01g(bs,s)
%   lid driven cavity, Geometry data
%
%   NE=bsp01g gives the number of boundary segment
%
%   D=bsp01g(bs) gives a matrix with one column for each
%   boundary segment specified in BS.
%   Row 1 contains the start parameter value.
%   Row 2 contains the end parameter value.
%   Row 3 contains the number of the left hand region.
%   Row 4 contains the number of the right hand region.
%
%   [X,Y]=bsp01g(BS,S) gives coordinates of boundary points.
%   BS specifies the boundary segments and S the
%   corresponding parameter values. BS may be a scalar.

% -- number of boundary segments ---------
nbs=3;
if nargin == 0,x = nbs; return, end

% d = [start par. value; end par. value;
%       left hand region; right hand region]

d = [...
% 1 2 3 
  0 0 0  ; 1 1 1  ; 1 1 1  ; 0 0 2];
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
        [X,Y,P] = bsp01f(K);
         x(ii) = interp1(P,X,s(ii));
         y(ii) = interp1(P,Y,s(ii));
      end
   end
end
function [X,Y,P] = bsp01f(segnr)
p = bsp01;
% boundary segments
% A = [X-Werte; Y-Werte];
switch segnr
case 1,
     EE = [1 2 3 4 5];
     A = p(:,EE);
case 2,
     EE = [...
     5, 12, 13, 14, 15, 16, 28, 40, 62, 70 ...
     79, 90,100,110,109,108,107,106,105,104 ...
     103,102,101, 91, 80, 71, 63, 41, 29, 17, 6 , 1];
     A = p(:,EE);
case 3
     EE = [45, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45];
     A = p(:,EE);
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
