function [gnew Gnew Gpnew]=LDSbackwardUpdate(g,G,f,F,A,CovH,meanH)
%LDSBACKWARDUPDATE Single Backward update for a Latent Linear Dynamical System (RTS smoothing update)
% [gnew Gnew Gpnew]=LDSbackwardUpdate(g,G,f,F,A,CovH,meanH)
%
% Inputs:
% g : smoothed mean p(h(t+1)|v(1:T))
% G : smoothed covariance p(h(t+1)|v(1:T))
% f : filterered mean p(h(t)|v(1:t))
% F : filterered covariance p(h(t)|v(1:t))
% A : transition matrix
% CovH : transition covariance
% CovV : emission covariance
% meanH : transition mean
%
% Outputs:
% gnew : smoothed mean p(h(t)|v(1:T))
% Gnew : smoothed covariance p(h(t)|v(1:T))
% Gpnew : smoothed cross moment  <h_t h_{t+1}|v(1:T)>
muh=A*f+meanH;
Shtptp=A*F*A'+CovH;
Shtpt=A*F;
leftA = (Shtpt')/Shtptp;
leftS = F - leftA*Shtpt;
leftm = f - leftA*muh;
gnew = leftA*g+leftm;
Gnew = leftA*G*leftA'+leftS; Gnew=0.5*(Gnew+Gnew'); % could also use Joseph's form if desired
Gpnew = leftA*G+gnew*g'; % smoothed <h_t h_{t+1}>