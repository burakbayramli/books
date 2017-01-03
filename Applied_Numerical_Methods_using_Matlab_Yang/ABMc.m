%ABMc.m: Predictor coefficients in Adams-Bashforth-Moulton method
clear
format rat
[l,L]=lagranp([-3 -2 -1 0],[0 0 0 0]); %Only coefficient polynomial L used  
for m=1:4
   iL=polyint(L(m,:)); 
   cAP(m)=polyval(iL,1)-polyval(iL,0); %integral over [0,1]
end
cAP 
[l,L]=lagranp([-2 -1 0 1],[0 0 0 0]); %Only coefficient polynomial L used
for m=1:4
   iL=polyint(L(m,:)); 
   cAC(m)=polyval(iL,1)-polyval(iL,0); %integral over [0,1]
end
cAC
format short