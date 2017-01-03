function [th,err,yi]=polyfits(x,y,N,xi,r)
%N   : the order of polynomial(>=0)
%r   : weighting factor array of the same dimension as y
M=length(x); 
if length(y)~=M, error('The numners of x,y must be equal'); end
x=x(:); y=y(:); %MAke all column vectors
if nargin==4
  if length(xi)==M
    r=xi; xi=min(x)+[0:100]/100*(max(x)-min(x)); %With input argument (x,y,N,r)
   else r=1;  %With input argument (x,y,N,xi)
  end 
 elseif nargin==3,
   xi=min(x)+[0:100]/100*(max(x)-min(x)); r=1; %With input argument (x,y,N)
end
no_con=0; %no constant (oth-order) term
if N<0, N=-N; no_con=1; end
A(:,N+1) =ones(M,1);
for n=N:-1:1, A(:,n)=A(:,n+1).*x; end
if length(r)==M
  %A0=A; y0=y; [Nr,Nc]=size(A0);
  for m=1:M
     A(m,:)=A(m,:)/r(m); y(m)=y(m)/r(m);
     V(m,m)=r(m)^2;
  end
  %if Nr>Nc, th_lscov=lscov(A0,y0,V), end %compare with lscov()
end
%if no_con==1, A=A(:,1:end-1); end
if no_con==1, A(:,end)=zeros(M,1); end
th=(A\y)';
ye= polyval(th,x); %estimated y values
err=norm(y-ye)/norm(y);
yi= polyval(th,xi);
if nargout==0, 
  [x,i]=sort(x); y=y(i); [xi,i]=sort(xi); yi=yi(i);
  plot(x,y,'k*',xi,yi,'b:')
end
