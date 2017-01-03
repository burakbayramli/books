function bits=binary(x,N)
% bits=binary(x,N)
% Input:
% x     =vector of integers >=0 with N bits/integer.
% Output:
% bits  = row vector with N*length(x) binary elements.
% Example:
% >> bits=binary([0,15,3], 4)
% bits =
%  0  0  0  0  1  1  1  1  0  0  1  1
x=row_vec(round(x));
if min(x)<0,
	error('x cannot have negative elements.');
elseif N<2 | N>32,
	error('N must be in the range [2,32].');
end
bits=rem(fix(row_vec(x'*2.^-[N-1:-1:0])),2);
