function dx=df_sat(t,x)
global G Me Re
dx=zeros(size(x)); 
r=sqrt(sum(x(1:2).^2));
if r<=Re, return; end % when colliding against the Earth surface
GMr3=G*Me/r^3;
dx(1)=x(3); dx(2)=x(4); dx(3)= -GMr3*x(1); dx(4)= -GMr3*x(2);
