%Program 3.5
function  [yi,S]=cspline(x,y,xi,KC,dy0,dyN)
%This function finds the cubic spline based on the input data points (x,y)
%Input:  x=[x0 x1 ... xN], y=[y0 y1 ... yN], xi=interpolation points
%        KC >=0 for descending order, <0 for ascending order
%        abs(KC)<=1 for derivative specified, =2 for 2nd derivative specified
%        abs(KC)=3 for 2nd derivative extrapolated
%        dy0 =S'(x0)=S01: initial derivative,  
%        dyN =S'(xN)=SN1: final derivative  
%Output: S(n,k); n=1:N, k=1,4
if nargin<6, dyN=0; end, if nargin<5, dy0=0; end
if nargin<4, KC=0; end 
N1 =length(x); N=N1-1; N_1=N-1;
% To construct the set of equations w.r.t. S(n,2), n=1,...,N+1
A =zeros(N1,N1); b=zeros(N1,1); 
S =zeros(N1,4);    %initialize the cubic spline coefficients
k=1:N; h(k)=x(k+1)-x(k); dy(k)=(y(k+1)-y(k))./h(k);
for m=2: N  %Eq.(3.34)
  A(m,m-1:m+1)=[h(m-1) 2*(h(m-1)+h(m)) h(m)];
  b(m)=3*(dy(m)-dy(m-1));
end
% Boundary condition
if abs(KC)<=1  %1st derivatives specified
  A(1,1:2)= [2*h(1) h(1)]; b(1)=3*(dy(1)-dy0); %Eq.(3.26-1)
  A(N1,N:N1)= [h(N) 2*h(N)]; b(N1)=3*(dyN-dy(N));%Eq.(3.26-2)
 elseif KC==2  %2nd derivatives specified
  A(1,1)=2; b(1)=dy0; A(N1,N1)=2; b(N1)=dyN; %Eq.(3.27)
 else %2nd derivatives extrapolated
  A(1,1:3)= [h(2) -h(1)-h(2) h(1)]; %Eq.(3.28)
  A(N+1,N-1:N+1)= [h(N) -h(N)-h(N-1) h(N-1)];
end
S(:,3)=A\b; 
for m =1:N %cubic spline coefficients 
  S(m,4) =(S(m+1,3)-S(m,3))/3/h(m);  %Eq.(3.30)
  S(m,2) =dy(m) -h(m)/3*(S(m+1,3)+2*S(m,3));
  S(m,1) =y(m);
end
%S(N1,1) =y(m);
if KC>=0, S=S(1:N, 4:-1:1); %descending order
 else S =S(1:N, 1:4); %ascending order
end
if nargin<3, yi=S;
 else yi=ppval(mkpp(x,S),xi);
end
