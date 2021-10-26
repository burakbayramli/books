function [x, DM] = ChebyshevCoeff(N, M)

%  The function [x, DM] =  chebdif(N,M) computes the differentiation
%  matrices D1, D2, ..., DM on Chebyshev nodes.
%
%  Input:
%  N:        Size of differentiation matrix.
%  M:        Number of derivatives required (integer).
%  Note:     0 < M <= N-1.
%
%  Output:
%  DM:       DM(1:N,1:N,ell) contains ell-th derivative matrix, ell=1..M.
%
%  The code implements two strategies for enhanced
%  accuracy suggested by W. Don and S. Solomonoff in
%  SIAM J. Sci. Comp. Vol. 6, pp. 1253--1268 (1994).
%  The two strategies are (a) the use of trigonometric
%  identities to avoid the computation of differences
%  x(k)-x(j) and (b) the use of the "flipping trick"
%  which is necessary since sin t can be computed to high
%  relative precision when t is small whereas sin (pi-t) cannot.
%  Note added May 2003:  It may, in fact, be slightly better not to
%  implement the strategies (a) and (b).   Please consult the following
%  paper for details:   "Spectral Differencing with a Twist", by
%  R. Baltensperger and M.R. Trummer, to appear in SIAM J. Sci. Comp.

%  J.A.C. Weideman, S.C. Reddy 1998.  Help notes modified by
%  JACW, May 2003.

I = eye(N);                             % Identity matrix.
L = logical(I);                         % Logical identity matrix.

n1 = floor(N/2); n2  = ceil(N/2);       % Indices used for flipping trick.

k = [0:N-1]';                           % Compute theta vector.
th = k*pi/(N-1);

x = sin(pi*[N-1:-2:1-N]'/(2*(N-1)));    % Compute Chebyshev points.

T = repmat(th/2,1,N);
DX = 2*sin(T'+T).*sin(T'-T);            % Trigonometric identity.
DX = [DX(1:n1,:); -flipud(fliplr(DX(1:n2,:)))];   % Flipping trick.
DX(L) = ones(N,1);                      % Put 1's on the main diagonal of DX.

C = toeplitz((-1).^k);                  % C is the matrix with
C(1,:) = C(1,:)*2; C(N,:) = C(N,:)*2;   % entries c(k)/c(j)
C(:,1) = C(:,1)/2; C(:,N) = C(:,N)/2;

Z = 1./DX;                              % Z contains entries 1/(x(k)-x(j))
Z(L) = zeros(N,1);                      % with zeros on the diagonal.

D = eye(N);                             % D contains diff. matrices.

for ell = 1:M
    D = ell*Z.*(C.*repmat(diag(D),1,N) - D);    % Off-diagonals
    D(L) = -sum(D');                            % Correct main diagonal of D
    DM(:,:,ell) = D;                            % Store current D in DM
end


