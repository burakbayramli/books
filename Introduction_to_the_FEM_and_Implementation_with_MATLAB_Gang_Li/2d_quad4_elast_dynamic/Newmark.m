% Time integration using Newmark scheme trapezoid rule
function [U,V,A]=Newmark(t,dt,beta,gamma,dim,K,M,C,F,bcsdisp,penalty,U,V,A)
n_nodes=size(F,1)/dim;
% get u,v,a vectors
u1= U(:,t-1); 
vel1 = V(:,t-1); 
accel1 = A(:,t-1);

% next 3 lines: compute the LHS matrix and RHS vector
LHS = M*(1.0/(beta*dt*dt))+ C*(gamma/(beta*dt)) + K; 
rhsvec = u1 + vel1*dt + accel1*((.5-beta)*dt*dt); 
RHS = F + M*(rhsvec*(1.0/(beta*dt*dt))) + C*(rhsvec*...
     (gamma/(beta*dt)) - vel1 - accel1*(dt*(1.0-gamma)));

% for-loop: apply displacement BC using the penalty method
for j=1:size(bcsdisp,1);    
  nid=bcsdisp(j,1);
  k=bcsdisp(j,2);
  RHS(dim*(nid-1)+k,1) =bcsdisp(j,3)*penalty;
  LHS(dim*(nid-1)+k,dim*(nid-1)+k)=penalty;
end

U(:,t)=LHS\RHS;  % solve for the displacement
% next two lines: calculated acceleration and velocity
A(:,t) = (U(:,t)- U(:,t-1))/(beta*dt*dt)... 
     -V(:,t-1)/(beta*dt) -A(:,t-1)*(0.5-beta)/beta;
V(:,t) =V(:,t-1) + A(:,t-1)*(1.0-gamma)*dt + A(:,t)*dt*gamma;