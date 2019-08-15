function Y = bsp02b(n,T,Y2,Y3,Parmeter3)
% Start trajectory forr Y in  Orbit problem
% Bryson-Ho, p. 66.
PP     = Parmeter3(2);
Y1     = PP + 0.5*Y3;
Y0     = [Y1; Y2; Y3];
t0     = 0; tf = T;
TSPAN  = linspace(tf,t0,n+1);
[T1,Y] = ode23(@bsp02c,TSPAN,Y0);
Y      = Y';
T1     = T1';
fliplr(Y); fliplr(T1);
flag = 0;
if flag == 1
   clf
   Y1 = Y(1,:);
   plot(T1,Y1,'r'),hold on
   plot(T1,Y1,'.','markersize',6), hold on
   Y2 = Y(2,:);
   plot(T1,Y2,'g'), hold on
   plot(T1,Y2,'.','markersize',6), hold on
   Y3 = Y(3,:);
   plot(T,Y3,'b'), hold on
   plot(T,Y3,'.','markersize',6), hold on
end

function Y = bsp02c(t,X);
% Start vector for Y in  Orbit problem
%  Bryson-Ho, p. 66.
BB = 1;
A  = [0, -1, 0; -1, 0, BB; 0, -2, 0];
Y  = A*X;

