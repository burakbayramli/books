function Y = timesArray( a, X )
%TIMESARRAY   Vectorizes the scalar-times-array operator .*.
%   More precisely, if a is an n*m array and X is a p*n*m array, then the
%   return value is
%       Y(i,j,k) = a(j,k)*X(i,j,k).
	
	if isscalar(a)
		Y = a*X;
	else
		% Make sure a is an 1*n*m array and X is a p*n*m array
		d = ndims(X);
		if ndims(a) == d-1
			a = reshape(a, [1, size(a)]);
		else
			assert(ndims(a)==d && size(a,1)==1);
		end

		% Repeat a p times in the first dimension and multiply
		Y = repmat(a, size(X,1), 1) .* X;
	end
end