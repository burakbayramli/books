function res = verspec(h,typ,t,dp,ds);
% Synopsis: res = verspec(h,typ,t,dp,ds).
% Verifies that a FIR filter meets the design specifications.
% Input parameters:
% h: the FIR filter coefficients
% other parameters: see description in firkais.m.
% Output:
% res: 0: OK, 1: increase order, 2: increase alpha.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

if (typ=='l'), ntest = 1;
Hp = abs(frqresp(h,1,100,[max(0,1.5*t(1)-0.5*t(2)),t(1)]));
Hs = abs(frqresp(h,1,100,[t(2),min(pi,1.5*t(2)-0.5*t(1))]));
elseif (typ=='h'), ntest = 1;
Hp = abs(frqresp(h,1,100,[t(2),min(pi,1.5*t(2)-0.5*t(1))]));
Hs = abs(frqresp(h,1,100,[max(0,1.5*t(1)-0.5*t(2)),t(1)]));
elseif (typ=='p'), ntest = 2;
Hp1 = abs(frqresp(h,1,100,[t(2),min(t(3),1.5*t(2)-0.5*t(1))]));
Hs1 = abs(frqresp(h,1,100,[max(0,1.5*t(1)-0.5*t(2)),t(1)]));
Hp2 = abs(frqresp(h,1,100,[max(t(2),1.5*t(3)-0.5*t(4)),t(3)]));
Hs2 = abs(frqresp(h,1,100,[t(4),min(pi,1.5*t(4)-0.5*t(3))]));
Hp = [Hp1; Hp2]; Hs = [Hs1; Hs2];
elseif (typ=='s'), ntest = 2;
Hp1 = abs(frqresp(h,1,100,[max(0,1.5*t(1)-0.5*t(2)),t(1)]));
Hs1 = abs(frqresp(h,1,100,[t(2),min(t(3),1.5*t(2)-0.5*t(1))]));
Hp2 = abs(frqresp(h,1,100,[t(4),min(pi,1.5*t(4)-0.5*t(3))]));
Hs2 = abs(frqresp(h,1,100,[max(t(2),1.5*t(3)-0.5*t(4)),t(3)]));
Hp = [Hp1; Hp2]; Hs = [Hs1; Hs2];
elseif (typ=='b'), ntest = 1;
Hp = abs(frqresp(h,1,100,[t,2*t])); Hs = zeros(1,100);
end

res = 0;
for i = 1:ntest,
   if(max(abs(Hp(i,1)-1),abs(Hp(i,100)-1)) > dp(i)), res = 1;
   elseif(max(Hs(i,1),Hs(i,100)) > ds(i)), res = 1; end
end
if (res), return, end
for i = 1:ntest,
   if(max(abs(Hp(i,:)-1)) > dp(i)), res = 2;
   elseif(max(Hs(i,:)) > ds(i)), res = 2; end
end

