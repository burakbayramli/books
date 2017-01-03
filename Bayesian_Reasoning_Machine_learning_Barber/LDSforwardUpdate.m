function [fnew Fnew logpvgv]=LDSforwardUpdate(f,F,v,A,B,CovH,CovV,meanH,meanV)
%LDSFORWARDUPDATE Single Forward update for a Latent Linear Dynamical System (Kalman Filter)
% [fnew Fnew logpvgv]=LDSforwardUpdate(f,F,v,A,B,CovH,CovV,meanH,meanV)
%
% Inputs:
% f : filterered mean p(h(t)|v(1:t))
% F : filterered covariance p(h(t)|v(1:t))
% v : observation v(t+1)
% A : transition matrix
% B : emission matrix
% CovH : transition covariance
% CovV : emission covariance
% meanH : transition mean
% meanV : emission mean
%
% Outputs:
% fnew : : filterered mean p(h(t+1)|v(1:t+1))
% Fnew : filterered covariance p(h(t+1)|v(1:t+1))
% logpgvg : log p(v(t+1)|v(1:t))
muh=A*f+meanH;
muv=B*muh+meanV;
Shh=A*F*A'+CovH;
Svv=B*Shh*B'+CovV;
Svh=B*Shh;
del = v-muv;
invSvvdel=Svv\del;
fnew = muh+Svh'*invSvvdel;
%Fnew=Shh-Svh'*(Svv\Svh); Fnew=0.5*(Fnew+Fnew');
K = Shh*B'/Svv; % Kalman Gain
tmp=eye(size(A))-K*B;
Fnew = tmp*Shh*tmp'+K*CovV*K'; % Joseph's form 
logpvgv = -0.5*del'*invSvvdel-0.5*log(det(Svv))-size(v,1)*0.5*log(2*pi);