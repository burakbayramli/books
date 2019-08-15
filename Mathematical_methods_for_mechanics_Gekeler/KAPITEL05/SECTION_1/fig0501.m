function fig0501
% Figure 5.1, 4 diagrams, one omitted
% Roots of x1 + x^TPx = 0; x2 + x^TQx = 0; x = [x1,x2]
% P = [p11,p12;p12,p22]; Q = [q11,q12;q12,q22]; 
% x1 notequal 0; y = x2/x1;
% p11 = P(1,1); p12 = P(1,2); p22 = P(2,2);
% q11 = Q(1,1); q12 = Q(1,2); qp22 = Q(2,2);
% f(y) = p22*y^3 + (2*p12 - q22)*y^2 + (p11 - 2q12)*y - q11

clc, format short, format compact
nr = 100; 
while ~ismember(nr,[1,2,3,4,5])
   nr   = input(' Example no. (1/2/3/4/5) ');
end;
%nr = 4;
rand('state',sum(100*clock));
B = rand(2,2); P = rand(2,2); Q = rand(2,2);
save datentst B P Q

switch nr
case 1
   P = [0,1;1,0];   Q = [-1,1;1,0]; B = [1,0; 0,1]; c = 0; SCALE = 3;
case 2 
   P = [1,0;0,1];   Q = [-1,1;1,0]; B = [1,0; 0,1]; c = 0; SCALE = 1.5;
case 3 
   P = [0,-1;-1,0]; Q = [-1,1;1,0]; B = [1,0; 0,1]; c = 0; SCALE = 2;
case 4 
   P = [3,0;0,1]; Q = [0.5,0;0,4]; B = [1,0; 0,1]; c = 0; SCALE = 0.5;
case 5   
   load datentst B P Q; c = 0; SCALE = 1;
end
clf
[X,RES] = fig0501aux(P,Q); X, RES  
LX = size(X,1);
plot_quad(P,B(1,:),c,SCALE,'b'), hold on
plot_quad(Q,B(2,:),c,SCALE,'g') 

for K = 1:LX
   if imag(X(K,1)) == 0
   circle(X(K,1),X(K,2),SCALE/50,'r',1); hold on
   end
end   
circle(0,0,SCALE/50,'k',1)

