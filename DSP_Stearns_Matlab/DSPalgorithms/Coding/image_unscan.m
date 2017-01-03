function x=image_unscan(y,M,N,K)
% x=image_unscan(y,M,N,K)
%
% Reverses the effect of image_scan to produce x.
% If K is not included, K=1 plane is assumed.
% See image_scan for definitions of terms.
y=row_vec(y);
if nargin<4,
    K=1;
end
a=zeros(M,N);
a(:)=1:M*N;
b=a(M:-1:1,:);
s=1;
for i=-(M-2):N-1,
	d=diag(b,i)';
	if(rem(i+M-1,2)==1)
		d=rev(d);
	end
	s=[s d];
end
x=zeros(M*N,K);
for k=1:K,
    x(s,k)=y(1+(k-1)*M*N:k*M*N)';
end
x=reshape(x,M,N,K);