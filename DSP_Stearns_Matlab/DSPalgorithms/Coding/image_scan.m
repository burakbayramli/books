function [y,M,N,K]=image_scan(x)
% [y,M,N,K]=image_scan(x)
%
% Zigzag scan of rectangular image x.
% x has K image planes, each with M rows and N columns of pixels.
% Scan begins at x(1,1,1) and proceeds in a zigxag pattern. Planes
% are scanned in order.
% 
% In each plane, y=row vector containing the scanned elements of x:
%  =[x(1,1),x(1,2),x(2,1),x(3,1),x(2,2),x(1,3),x(1,4),x(2,3),...].
%
% See also image_unscan
[M,N,K]=size(x);
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
y=zeros(K,M*N);
for i=1:K,
   y(i,:)=x(s+(i-1)*M*N);
end
y=reshape(y',[1 M*N*K]);