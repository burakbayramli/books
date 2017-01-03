function [xo,fo]=opt_quad0(f,x012,f012,TolX,TolFun,k)
x0= x012(1); x1= x012(2); x2= x012(3);
f0= f012(1); f1= f012(2); f2= f012(3);
nd= [f0-f2 f1-f0 f2-f1]*[x1*x1 x2*x2 x0*x0; x1 x2 x0]'; 
x3= nd(1)/2/nd(2); f3=feval(f,x3); %Eq.(7.1-4)
if k<=0|abs(x3-x1)<TolX|abs(f3-f1)<TolFun
  xo=x3;  fo=f3;
if k==0, fprintf('Just the best in given # of iterations'), end
else 
  if x3<x1
    if f3<f1, x012=[x0 x3 x1]; f012= [f0 f3 f1];
     else  x012=[x3 x1 x2]; f012= [f3 f1 f2];
    end
else
    if f3<=f1, x012=[x1 x3 x2]; f012= [f1 f3 f2];
     else  x012=[x0 x1 x3]; f012= [f0 f1 f3];
    end
  end
 [xo,fo]=opt_quad0(f,x012,f012,TolX,TolFun,k-1);
end
