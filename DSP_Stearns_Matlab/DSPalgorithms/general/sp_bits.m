function b=sp_bits(x,N)
% b=sp_bits(x,N)
% Input:
% x     =vector of integers >=0 with N bits/integer.
% Output:
% b     = row vector with N*length(x) binary elements.
% Example:
% >> b=sp_bits([0,15,3], 4)
% b=
%  0  0  0  0  1  1  1  1  0  0  1  1
x=double(row_vec(round(x)));
if min(x)<0,
	error('x cannot have negative elements.');
elseif N<1 | N>32,
	error('N must be in the range [1,32].');
end
b=rem(fix(row_vec(x'*2.^-[N-1:-1:0])),2);
