%ABMc1.m: Predictor coefficients in Adams-Bashforth-Moulton method
clear
format rat
%Another way to get the ABM coefficients
%y(k+1)=y(k)+h*f(k)+h^2/2*f'(k)+h^3/3!*f"(k)+h^4/4!*f"'(k)+h^5/5!*f^(3)(k)+..
for i=1:3
   [ci,erri]=difapx(i,[-3 0]); c(i,:)=ci; err(i)=erri;
end
cAP=[0 0 0 1]+[1/2 1/6 1/24]*c, errp=-[1/2 1/6 1/24]*err'+1/120
%y(k+1)=y(k)-h*f(k+1)+h^2/2*f'(k+1)+h^3/3!*f"(k)+h^4/4!*f"'(k)+h^5/5!*f^(3)(k)+..
cAC=[0 0 0 1]+[-1/2 1/6 -1/24]*c, errc=-[-1/2 1/6 -1/24]*err'+1/120
format short