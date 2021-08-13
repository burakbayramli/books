function xh=hydro(xst,nu,np,qmethod)
%HYDRO removes hydrostatic pressure from solution vector
%  xh=hydro(xst,nu,np,qmethod);
%   input
%         xst          stokes solution
%         nu,np        velocity, pressure space dimensions  
%         qmethod      mixed method switch
%   IFISS function: DJS; 2 January 2011.
% Copyright (c) 2009 D.J. Silvester, V. Simoncini 
u=xst(1:nu);p=xst(nu+1:nu+np);
if qmethod<3,
      ph=mean(p); xh=[u;p-ph];
elseif qmethod==3,
      pph=mean(p(1:3:end)); pp=p; pp(1:3:end)=pp(1:3:end)-pph; xh=[u;pp];    
end
return
