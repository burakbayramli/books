function [ZB,TB] = cauchybc(p,t,T,RCZ,RCT,Parmeter)
% Rightside for Vorticity
% Natural boundary for stream function and temperature
BETA = Parmeter(3); KAPPA = Parmeter(4); T_AIR = Parmeter(5);
g = Parmeter(6);
X = p(1,:); Y = p(2,:); M = size(t,2); N = size(p,2);
WB = zeros(N,N); ZB = zeros(N,1); TB = ZB;
if ~isempty(RCZ)
   for I = 1:size(RCZ,2)
      K = RCZ(1:2,I);
      [ME,BE] = ralell(p(1,K),p(2,K));
      ZB(K)   = ZB(K)+ BE*RCZ(3,I);
     % MZ(K,K) = MZ(K,K) + ME;
   end
end
if ~isempty(RCT)
   for I = 1:size(RCT,2)
      K = RCT(1:2,I);
      [ME,BE] = ralell(p(1,K),p(2,K));
      TB(K)   = TB(K) - KAPPA*ME*(T(K) - T_AIR);
   end
end
