function y = clamp( x, range )
%CLAMP   Force x into the interval [range(1), range(2)].

	y = max(range(1), min(range(2), x));
	
end