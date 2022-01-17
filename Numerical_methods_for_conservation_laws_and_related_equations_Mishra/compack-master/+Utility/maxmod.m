function ret = maxmod( a, b, c )
%MAXMOD

	if nargin == 3
		ret = maxmod(maxmod(a, b), maxmod(b, c));
		return;
	end
	
	ret = (sign(a) == sign(b)) .* sign(a) .* max(abs(a), abs(b));

end

