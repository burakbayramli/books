function ret = minmod( a, b, c )
%MINMOD

	if nargin == 3
		ret = Utility.minmod(Utility.minmod(a, b), Utility.minmod(b, c));
		return;
	end
	
	ret = (sign(a) == sign(b)) .* sign(a) .* min(abs(a), abs(b));

end