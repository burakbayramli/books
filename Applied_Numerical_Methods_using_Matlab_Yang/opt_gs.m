function [xo,fo]=opt_gs(f,a,b,r,TolX,TolFun,k)
h=b-a; rh=r*h; c=b-rh; d=a+rh; 
fc=feval(f,c); fd=feval(f,d); 
if k<=0|(abs(h)<TolX&abs(fc-fd)<TolFun)
  if fc<=fd, xo=c;  fo=fc;  
   else  xo=d; fo=fd; 
  end
if k==0, fprintf('Just the best in given # of iterations'), end
else 
  if fc<fd, [xo,fo]=opt_gs(f,a,d,r,TolX,TolFun,k-1);
   else  [xo,fo]=opt_gs(f,c,b,r,TolX,TolFun,k-1);
  end
end
