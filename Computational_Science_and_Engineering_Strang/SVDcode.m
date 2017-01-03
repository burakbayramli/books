%1.7  SVDcode.m

%SVDcode not fast or numerically stable

% input A, output orthogonal U,V and diagonal sigma with A=U*sigma*V'
[m,n]=size(A); r=rank(A); [V,squares]=eig(A'*A); % n by n matrices
sing=sqrt(squares(1:r,1:r)); % r by r, singular values >0 on diagonal
sigma=zeros(m,n); sigma(1:r,1:r)=sing; % m by n singular value matrix
UU=A*V(:,1:r)*inv(sing); % first r columns of U (singular vectors)
[U,R]=qr(UU); U(:,1:r)=UU; % qr command completes UU to an m by m U
A-U*sigma*V'; % test for zero m by n matrix (could print its norm)
