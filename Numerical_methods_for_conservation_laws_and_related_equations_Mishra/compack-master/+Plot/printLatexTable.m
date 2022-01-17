function printLatexTable( x, signifDig )
%PRINTLATEXTABLE   Print a matrix in LaTeX table form.
% Input:
% x:			Table to print
% signifDig:	(Optional) Number of significant digits to print. Default
%				value is 4.

	if nargin < 2
		signifDig = 4;
	end
	format = sprintf('%%.%dg', signifDig);

	[ny, nx] = size(x);
	for i=1:ny
		for j = 1:nx-1
			fprintf([format, '  &  '], x(i,j));
		end
		fprintf([format, '  \\\\\n'], x(i,nx));
	end
	
end

