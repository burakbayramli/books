%Program 3.3
function [c,x,y]=cheby(f,N,a,b)
%Input : f=function name on [a,b]
%Output: n=Newton polynomial coefficients of order N
%           [x,y]=Chebyshev nodes
if nargin==2,  a=-1; b=1;  end
k =[0: N];
theta =(2*N+1-2*k)*pi/(2*N+2);
xn =cos(theta);                                 %Eq.(3.12-1)
x =(b-a)/2*xn +(a+b)/2;                    %Eq.(3.12-2)
y =feval(f,x);
d(1) =y*ones(N+1,1)/(N+1);
for m=2: N+1  
   cos_mth =cos((m-1)*theta);
   d(m) =y*cos_mth'*2/(N+1);            %Eq.(3.17-3)
end
xn =[2/(b-a)  -(a+b)/(b-a)];             %Eq.(3.12-2)
T_0 =1;               T_1 =xn;                  %Eq.(3.14-2)
c =d(1)*[0  T_0] +d(2)*T_1;              %Eq.(3.16)
for  m=3: N+1
   tmp =T_1;
   T_1 =2*conv(xn,T_1) -[0  0  T_0];  %Eq.(3.14-1)
   T_0 =tmp;
   c =[0  c] +d(m)*T_1;                        %Eq.(3.16)
end