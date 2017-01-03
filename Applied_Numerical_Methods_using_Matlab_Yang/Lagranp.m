%Program 3.1
function [l,L]=lagranp(x,y)
%Input : x=[x0 x1 ... xN], y=[y0 y1 ... yN]
%Output: l=Lagrange polynomial coefficients of order N
%            L=Lagrange coefficient polynomial
N= length(x)-1; %the order of polynomial
l=0;
for m=1:N+1
   P=1;
   for k=1:N+1
     if k~=m
      P=conv(P,poly(x(k)))/(x(m)-x(k));
     end
   end
   L(m,:)=P; %Lagrange coefficient polynomial
   l =l+y(m)*P; %Lagrange polynomial
end
