function y = randgen(p, m, n, s)
% RANDGEN	Generates discrete random variables given the pdf
%
% Y = RANDGEN(P, M, N, S)
%
% Inputs :
%	P : Relative likelihoods (or pdf) of symbols
%	M : Rows    <Default = 1>
%	N : Columns <Default = 1>
%	S : Symbols <Default 1:length(P)>

% Change History :
% Date		Time		Prog	Note
% 06-Jul-1997	 4:36 PM	ATC	Created under MATLAB 5.0.0.4064

% ATC = Ali Taylan Cemgil,
% Bogazici University, Dept. of Computer Eng. 80815 Bebek Istanbul Turkey
% e-mail : cemgil@boun.edu.tr 
if isempty(p); y=[]; return; end % no samples if p is empty (DB)
if (nargin < 2) m = 1; end
if (nargin < 3) n = 1;end
if (nargin < 4) s = 1:length(p);end

c = cumsum(p);
c = c(:)'/c(end);
N = m*n;
u = rand(N,1);
% for i=1:N, y(i) = length(find(c<y(i)))+1; end;

y = sum( c(ones(N,1),:) < u(:, ones(length(c),1)) , 2 ) + 1;

y = reshape(s(y), m, n);
