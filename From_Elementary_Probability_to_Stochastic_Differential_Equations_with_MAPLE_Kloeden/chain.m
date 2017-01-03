%CHAIN  Test stochastic Chain Rule
%
% Solve SDE for V(X) = sqrt(X) where X solves 
%         dX = (alpha - X) dt + beta sqrt(X) dW,   X(0) = Xzero,
%      with alpha = 2, beta = 1 and Xzero = 1.
% Xem1 is Euler-Maruyama solution for X. 
% Xem2 is Euler-Maruyama solution of SDE for V from Chain Rule.
% Hence, we compare sqrt(Xem1) and Xem2.
% Note: abs is used for safety inside sqrt, but has no effect in this case.

randn('state',100)
alpha = 2; beta = 1; T = 1; N = 200; dt = T/N;  % Problem parameters
Xzero = 1; Xzero2 = sqrt(Xzero);                % 

Dt = dt;                                        % EM steps of size Dt = dt
Xem1 = zeros(1,N); Xem2 = zeros(1,N);           % preallocate for efficiency 
Xtemp1 = Xzero;  Xtemp2 = Xzero2;
for j = 1:N
   Winc = sqrt(dt)*randn;  
   f1 = (alpha-Xtemp1);
   g1 = beta*sqrt(abs(Xtemp1));
   Xtemp1 = Xtemp1 + Dt*f1 + Winc*g1;
   Xem1(j) = Xtemp1;
   f2 = (4*alpha-beta^2)/(8*Xtemp2) - Xtemp2/2;
   g2 = beta/2;
   Xtemp2 = Xtemp2 + Dt*f2 + Winc*g2;
   Xem2(j) = Xtemp2;
end

plot([0:Dt:T],[sqrt([Xzero,abs(Xem1)])],'b-',[0:Dt:T],[Xzero,Xem2],'ro')
legend('Direct Solution','Solution via Chain Rule',2)
xlabel('t','FontSize',12) 
ylabel('V(X)','FontSize',16,'Rotation',0,'HorizontalAlignment','right')

Xdiff = norm(sqrt(Xem1) - Xem2,'inf')
