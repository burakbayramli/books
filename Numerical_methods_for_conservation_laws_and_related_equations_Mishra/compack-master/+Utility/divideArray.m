function Y = divideArray( X, a )
%DIVIDEARRAY   Vectorizes the scalar-divide-array operator ./.
%   More precisely, if a is an n*m array and X is a p*n*m array, then the
%   return value is
%       Y(i,j,k) = X(i,j,k) / a(j,k).

	% Make sure a is an 1*n*m array and X is a p*n*m array
	d = ndims(X);
	if ndims(a) == d-1
		a = reshape(a, [1, size(a)]);
	else
		assert(ndims(a)==d && size(a,1)==1);
	end
	
	% Repeat a p times in the first dimension and divide
	Y = X ./ repmat(a, size(X,1), 1);
end