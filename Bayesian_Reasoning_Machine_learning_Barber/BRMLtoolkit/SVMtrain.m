function [A,G] = SVMtrain(Q,y,C)
%SVMTRAIN train a Support vector Machine
% compute the SMO decomposition algorithm from Fan et al JMLR 2005
%
% Inputs:
%       Q_ij    -   y_i y_j K_ij  (Where K is the kernel)
%       y       -   labels
%       C       -   C parameter
%
% Outputs:
%       A       -   alpha values
%       G       -   Gradient
% written by Zakria Hussain, UCL, Jul 2008
eps = 1e-3; tau = 1e-12;
len = length(y);
A = zeros(len,1); G = -ones(len,1);

while(1)
    [i,j] = selectB(Q,G,A,y,C,tau,eps);
    if j == -1;  break;  end
    
    % working set is (i,j)
    a = Q(i,i)+Q(j,j)-2*y(i)*y(j)*Q(i,j);
    if a <= 0;  a = tau;   end
    b = (-y(i)*G(i))+(y(j)*G(j));
    
    % update alpha
    oldAi = A(i); oldAj = A(j);
    A(i) = A(i) + (y(i)*b/a);
    A(j) = A(j) - (y(j)*b/a);
    
    % project alpha back to the feasible region
    sum = (y(i)*oldAi) + (y(j)*oldAj);
    if A(i) > C;  A(i) = C; end
    if A(i) < 0;  A(i) = 0; end
    
    A(j) = y(j)*(sum-y(i)*A(i));
    
    if A(j) > C; A(j) = C; end
    if A(j) < 0; A(j) = 0; end
    
    A(i) = y(i)*(sum-y(j)*A(j));
    
    % update gradient
    deltaAi = A(i) - oldAi; deltaAj = A(j) - oldAj;
    G(:) = G(:) + (Q(:,i)*deltaAi) + (Q(:,j)*deltaAj);
end

function [i,j] = selectB(Q,G,A,y,C,tau,eps)
len = length(A);
i = -1; Gmax = -inf; Gmin = inf;
for t=1:len
    if (y(t)==1 && A(t)<C) || (y(t)==-1 && A(t)>0)
        if -y(t)*G(t) >= Gmax
            i = t;
            Gmax = -y(t)*G(t);
        end
    end
end
j = -1; obj_min = inf;
for t=1:len
    if (y(t)==1 && A(t)>0) || (y(t)==-1 && A(t)<C)
        b = Gmax + (y(t)*G(t));
        if -y(t)*G(t) <= Gmin ; Gmin = -y(t)*G(t);  end
        if b>0
            a = Q(i,i)+Q(t,t)-2*y(i)*y(t)*Q(i,t);
            if a<=0; a = tau; end
            if -(b*b)/a <= obj_min
                j=t;
                obj_min = -(b*b)/a;
            end
        end
    end
end
if Gmax-Gmin < eps
    i = -1; j = -1;
end