function [cw] = lagrangeweights(x)
% Purpose: Compute weights for Taylor expansion of Lagrange polynomial based
%          on x and evaluated at 0.
%          Method due to Fornberg (SIAM Review, 1998, 685-691)
np = length(x); cw=zeros(np,np);
cw(1,1)=1.0; c1 = 1.0; c4 = x(1);
for i=2:np
    mn = min(i,np-1)+1;
    c2 = 1.0; c5 = c4; c4 = x(i);
    for j=1:i-1
      c3 = x(i)-x(j); c2 = c2*c3;
      if (j==i-1)
          for k=mn:-1:2
              cw(i,k) = c1*((k-1)*cw(i-1,k-1)-c5*cw(i-1,k))/c2;
          end
          cw(i,1) = -c1*c5*cw(i-1,1)/c2;
      end
      for k=mn:-1:2
          cw(j,k) = (c4*cw(j,k)-(k-1)*cw(j,k-1))/c3;
      end
      cw(j,1) = c4*cw(j,1)/c3;
    end
    c1=c2;
end
return