function [T,J] = pitcon2(F,WEG,TANG,Parmeter)
% Calculation of tangent and of index J for PITCON
% last tangent is chosen as row vector E ,
% which must NOT be perpendicular to the new tangent
% -- Parameter -------------
MU = 0.2;
% --------------------------
[N1,M]  = size(WEG);
GRAD    = feval(F,WEG(:,M),2,Parmeter);
E       = TANG(:,M-1)';
A       = [GRAD;E];
R       = zeros(N1,1); R(N1) = 1;
W       = A\R;
T       = W/norm(W);
ABST    = abs(T);
ABSTANG = abs(TANG);
[AUX,I] = sort(ABST);
J       = I(N1); J1 = I(N1-1);
if ABST(J) < ABSTANG(J,M-1) & ABST(J1) > ABSTANG(J1,M-1) ...
& ABST(J1) >= MU*ABST(J), J = J1; end
sigma = 1;
%if sign(T(J)) == sign(TANG(J,M-1)), sigma = 1; end
%sigma
T = sigma*T;
