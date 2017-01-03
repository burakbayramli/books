%MIL  Milstein method on 3D finance SDE
%
% SDE is  dX(1) = X(1) X(2) dW(1)                             X(1)_0 = 1
%         dX(2) = -(X(2)-X(3)) dt + 0.3 X(2) dW(2)            X(2)_0 = 0.1
%         dX(3) = (X(2)-X(3)) dt                              X(3)_0 = 0.1
%
% Discretized Brownian path over [0,1] has delta = 2^(-18).
% Milstein timestep is Delta = sqrt(delta).
% Substeps for double integral are of size delta.

clf
randn('state',1)
T = 1; Delta = 2^(-9); delta = Delta^2;         
L = T/Delta; K = Delta/delta;

X1 = zeros(1,L+1); X2 = zeros(1,L+1); X3 = zeros(1,L+1);
Y2 = 0;

X1(1) = 1;
X2(1) = 0.1;
X3(1) = 0.1;
for j = 1:L
    Y1 = 0; Winc1 = 0; Winc2 = 0;
    for k = 1:K
        dW1 = sqrt(delta)*randn;
        dW2 = sqrt(delta)*randn;
        Y1 = Y1 + Y2*dW1;
        Y2 = Y2 + dW2;
        Winc1 = Winc1 + dW1;
        Winc2 = Winc2 + dW2;
    end
    X1(j+1) = X1(j) + X1(j)*X2(j)*Winc1 + ...
                 X1(j)*(X2(j)^2)*0.5*(Winc1^2 - Delta) +...
                  0.3*X1(j)*X2(j)*Y1;
    X2(j+1) = X2(j) - (X2(j) - X3(j))*Delta + 0.3*X2(j)*Winc2 + ...
             0.9*X2(j)*0.5*(Winc2^2 - Delta);
    X3(j+1) = X3(j) + (X2(j) - X3(j))*Delta;
end
plot([0:Delta:T],X1,'r-'), hold on
plot([0:Delta:T],X2,'bl--')
plot([0:Delta:T],X3,'b-.')
legend('X^1','X^2','X^3')
xlabel('t','FontSize',16), ylabel('X','FontSize',16)
