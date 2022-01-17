function ret = avg( x, n )
%AVG   Averages neighboring values in a given direction
%	Similar to diff(), but instead of the difference, computes the arithmetic
%	average of neighboring values.

	narginchk(1, 2);
	if nargin == 1
		n = 1;
	end
	assert(n <= length(size(x)));
		
	s = size(x);
	ne = s(n);
	if ne == 1
		% If there is only one element in direction n, then do nothing.
		ret = x;
		return;
	end
	
	% The idea is to resize x so that dimension n becomes dimension 2, then
	% average in dimension 2, and then resize it back again.
	s1 = prod(s(1:n-1));
	s3 = prod(s(n+1:end));
	x = reshape(x, [s1, ne, s3]);
	
	ret = (x(:,1:ne-1,:) + x(:,2:ne,:)) / 2;
	newS = s;	newS(n) = ne-1;
	ret = reshape(ret, newS);

end

