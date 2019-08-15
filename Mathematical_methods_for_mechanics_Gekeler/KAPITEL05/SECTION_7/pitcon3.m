function h = pitcon3(F,WEG,TANG,J,Parmeter)
% Calculation of step length for predictor
% -- Parameter -----------
kapmin  = 0.01;
epsmin  = 0.01;
chi     = 3;
hmin    = 0.01;
hmax    = 0.5;
ss      = 0.5;   % TODO-TODO
M       = size(WEG,2);
% ----------------------------
deltas1 = norm(WEG(:,M-1) - WEG(:,M-2));
deltas2 = norm(WEG(:,M)   - WEG(:,M-1));
w1      = (TANG(:,M-1)    - TANG(:,M-2))/deltas1;
w2      = (TANG(:,M)      - TANG(:,M-1))/deltas2;
kapaux  = norm(w2) + deltas2*(norm(w2) - norm(w1))/(deltas1 + deltas2);
kappa   = max(kapmin,kapaux);
eps1    = ss;
if ss <= epsmin*deltas2, eps1 = epsmin*deltas2; end
if ss >= deltas2, eps1 = deltas2; end
h1      = sqrt(2*eps1/kappa);
h2      = 1 - TANG(J,M-2)/TANG(J,M-1);
h3      = h1*(1 + h1*h2/(2*deltas2));
h4      = max([h3,hmin,deltas2/chi]);
h       = min([h4,hmax,chi*deltas2]);
