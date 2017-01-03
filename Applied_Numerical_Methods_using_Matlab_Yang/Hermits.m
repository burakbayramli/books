function H=hermits(x,y,dy,KC)
% finds Hermite interpolating polynomials
%Input : [x,y],dy - points and derivatives at the points
%Output: H=coefficients of cubic Hermite interpolating polynomial 
if nargin<4,  KC=0;  end
N=length(x)-1;
for n=1:N
 H(n,:)=hermit(0,y(n),dy(n),x(n+1)-x(n),y(n+1),dy(n+1),KC);
end