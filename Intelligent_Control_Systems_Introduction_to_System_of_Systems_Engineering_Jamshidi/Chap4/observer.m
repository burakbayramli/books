function observer


M = .9153;
m = 0.207;
b = 0.3;
i = 0.06559;
g = 9.8;
l = 0.975;

p = i*(M+m)+M*m*l^2; %denominator for the A and B matricies

A = [0      1              0           0;
     0 -(i+m*l^2)*b/p  (m^2*g*l^2)/p   0;
     0      0              0           1;
     0 -(m*l*b)/p       m*g*l*(M+m)/p  0];

B = [     0; 
    (i+m*l^2)/p;
          0;
      m*l/p];
       
C = [1 0 0 0;
     0 0 1 0];
       
D = [0;
     0];
     
Ts=1/100;

[F,G,H,J]=c2dm (A,B,C,D,Ts,'zoh')

co = ctrb (F,G);
ob = obsv (F,H);

Controllability = rank (co)
Observability = rank (ob)
T=0:0.01:5;
U=0.2*ones(size(T));
T=0:0.01:5;
U=0.2*ones(size(T));

x=5000	;	%weighting factor for the cart position
y=100;	%weighting factor for the pendulum angle

Q=[x 0 0 0;
   0 0 0 0;
   0 0 y 0;
   0 0 0 0];

R = 1;

K = dlqr(F,G,Q,R);


Nbar = -048.55;

poles = eig (F-G*K)
P = [-0.3 -0.31 -0.32 -0.33];

L = place (F',H',P)'
K = dlqr(F,G,Q,R)


Fce = [F-G*K		G*K;
       zeros(size(F)) 	(F-L*H)];
       
Gce = [G*Nbar;
       zeros(size(G))];
       
Hce = [H zeros(size(H))];

Jce = [0;0];

[Y,X] = dlsim (Fce,Gce,Hce,Jce,U);

stairs (T,Y)
legend ('cart (x)','pendulum (phi)')


