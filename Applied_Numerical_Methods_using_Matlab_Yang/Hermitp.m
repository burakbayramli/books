function p=Hermitp(N)
%Hn+1(x)=2xHn(x)-Hn'(x) from 'Advanced Engineering Math' by Kreyszig     
if N<=0,  p=1;
 else 
   p=[2 0];
   for n=2:N, p= 2*[p 0]-[0 0 drvtp(p)]; end
end
%elseif N==0, p=1; 
%elseif N==1, p=[1 0];
%else 
%    p=zeros(1,N+1);
%    p(1)=2^n; m=1;
%    for n=1:2:N-1
%       p(n+2)=-p(n)*(N-n+1)*(N-n)/4/m;
%       m= m+1;
%    end
%end

%Hn'(x)=2nHn-1(x)
% if mod(N,2)==0,
%   p(N+1)=1;
%   for n=N:-1:N/2+1, p(N+1)=-p(N+1)*n;  end
% end
% if N==0, p=1;
%  else p=intgrlp(2*N*Hermitp(N-1));
% end
