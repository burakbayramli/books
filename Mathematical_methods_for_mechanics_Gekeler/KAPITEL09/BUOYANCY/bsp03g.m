function [x,y] = bsp03g(bs,s)
%   benard cell, geometry data
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
nbs=37;

if nargin == 0,x = nbs; return, end

% d = [start par. value; end par. value;
%       left hand region; right hand region]
d1 = [...
% 1 2 3 4  5 6 7  8 9 10  11 12 13  14 15 16  17 18 19
  0 0 0 0  0 0 0  0 0  0   0  0  0   0  0  0   0  0  0;
  1 1 1 1  1 1 1  1 1  1   1  1  1   1  1  1   1  1  1;
  1 1 1 1  2 2 2  3 3  3   4  4  4   5  5  5   6  6  6
  0 2 0 0  0 3 0  0 4  0   0  5  0   0  6  0   0  7  0];

d2 = [...
% 20 21 22  23 24 25  26 27 28  29 30 31  32 33  34 35 36 37
   0  0  0   0  0  0   0  0  0   0  0  0   0  0   0  0  0  0;
   1  1  1   1  1  1   1  1  1   1  1  1   1  1   1  1  1  1;
   7  7  7   8  8  8   9  9  9  10 10 10  11 11  11 12 12 12;
   0  8  0   0  9  0   0 10  0   0 11  0   0 12   0  0  0  0];
d = [d1,d2];
% -- boundary segments ----------------
bs1 = bs(:).';
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
        [X,Y,P] = bsp03f(K);
         x(ii) = interp1(P,X,s(ii),'linear');
         y(ii) = interp1(P,Y,s(ii),'linear');
      end
   end
end
