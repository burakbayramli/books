function err=err_of_sol_de(df,t,x,varargin)
% evaluate the error of solutions of differential equation
[Nt,Nx]=size(x); if Nt<Nx, x=x.'; [Nt,Nx]= size(x); end
n1=2:Nt-1; t=t(:); h2s= t(n1+1)-t(n1-1);
dx= (x(n1+1,:)-x(n1-1,:))./(h2s*ones(1,Nx));
num=x(n1+1,:)-2*x(n1,:)+x(n1-1,:); den=(h2s/2).^2*ones(1,Nx);
d2x=num./den;
for m=1:Nx 
  for n=n1(1):n1(end)  
    dfx=feval(df,t(n),[x(n,m) dx(n-1,m)],varargin{:}); 
    errm(n-1,m)=d2x(n-1,m)-dfx(end);
  end
end
err=sum(errm.^2)/(Nt-2);
