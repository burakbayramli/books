function [xo,fo]=opt_quad(f,x0,TolX,TolFun,MaxIter)
%search for the minimum of f(x) by quadratic approximation method
if length(x0)>2, x012=x0(1:3);
 else
   if length(x0)==2, a=x0(1); b=x0(2);
    else a=x0-10; b=x0+10;
   end
   x012= [a (a+b)/2 b];
end
f012= f(x012);
[xo,fo]=opt_quad0(f,x012,f012,TolX,TolFun,MaxIter);
