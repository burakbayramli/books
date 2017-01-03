function INTfxy=int2s(f,a,b,c,d,M,N) 
%double integral of f(x,y) over R={(x,y)|a<=x<=b,c(x)<=y<=d(x)} using Simpson rule
if ceil(M)~=floor(M) %fixed width of subinterval on x
   hx=M; M=ceil((b-a)/hx);
end
if mod(M,2)~=0, M=M+1; end
hx=(b-a)/M; m=1:M+1; x=a+(m-1)*hx;

if isnumeric(c), cx(m)=c;
 else  cx(m)=feval(c,x(m));
end  
if isnumeric(d), dx(m)=d; 
 else  dx(m)=feval(d,x(m));   
end
if ceil(N)~=floor(N) %fixed width of subinterval on y
  hy=N;  
  Nx(m)=ceil((dx(m)-cx(m))/hy); 
  ind=find(mod(Nx(m),2)~=0); Nx(ind)=Nx(ind)+1;  
 else   %fixed number of subintervals
  if mod(N,2)~=0,  N=N+1;  end 
  Nx(m)=N;
 end
for m=1:M+1
  sx(m)= smpsns_fxy(f,x(m),cx(m),dx(m),Nx(m));  
end
kodd=2:2:M; keven=3:2:M-1; %the set of odd/even indices
INTfxy= hx/3*(sx(1)+sx(M+1)+4*sum(sx(kodd))+2*sum(sx(keven)));  