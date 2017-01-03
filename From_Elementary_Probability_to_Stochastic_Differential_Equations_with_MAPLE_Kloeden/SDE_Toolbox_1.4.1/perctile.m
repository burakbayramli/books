function y = perctile(x, p)

%   PERCTILE Percentiles.
%
%   Y = PERCTILE(X, P) returns the Pth percentiles of the data in X.  If P
%   is a scalar, Y is a vector with the Pth percentile of each column of X.
%   If P is a vector, each row in Y corresponds to each element in P.
%
%   The PERCTILE command differs from the PRCTILE command (Stats Toolbox) in
%   the way that it faster and uses a slightly different algorithm.  The
%   difference in algorithm causes the results from the two command to
%   differ slightly, especially for small samples.
%
%   See also PRCTILE, TRIMMEAN.

%   Author:      Peter J. Acklam
%   Time-stamp:  2004-09-22 19:13:44 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   error(nargchk(2, 2, nargin));
   if ndims(x) > 2
      error('X can not have more than two dimensions.');
   end

   x = sort(x);                         % sort the data matrix

   [mx, nx] = size(x);                  % get the size of the data matrix
   if min(mx, nx) == 1                  % if data matrix is a vector
      x = x(:);                         %   make it a column vector
      [mx, nx] = size(x);               %   and get the new size
   end

   [mp, np] = size(p);
   if min(mp, np) > 1
      error('P must be a scalar or a vector.');
   end

   p  = 0.01*p(:);              % make it a column vector and normalize
   mp = max(mp, np);
   %np = min(mp, np);           % ("np" is not used)

   c = (mx-1)*p + 1;            % calculate "indices"
   k = c == round(c);           % find integral indices

   y = zeros(mp,nx);            % initialize output matrix

   % For the values of C that are integers, we can assign the percentiles
   % directly.
   i = find(k);
   if ~isempty(i)
      y(i,:) = x(c(i),:);
   end

   % For the values of C that are not integers, we use a linear combination
   % of the two closest values.
   i = find(~k);
   if ~isempty(i)
      il = floor(c(i));
      iu = ceil(c(i));
      wl = iu - c(i);
      wu = c(i) - il;
      y(i,:) = wl(:,ones(1,nx)).*x(il,:) + wu(:,ones(1,nx)).*x(iu,:);
   end
