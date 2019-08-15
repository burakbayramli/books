function [THETA1,THETA2] = roots_f(a,b,alf,bet)
% berechnet Grenzwinkel als Nullstellen von
% f(x) = (alf - bet*x)*(1 - x*x) - (a - b*x)^2;

RR = roots(bet,-alf-b*b,2*a*b-bet,alf-a*a);
J  = find(imag(RR) ~= 0);
if isempty(J)
   K   = find(abs(RR) <= 1);
   RR1 = RR(K(1)); RR2 = RR(K(2));
else
   disp('unphysikalische Eingabe')
   break
end
if RR2 <= RR1, AUX = RR1; RR1 = RR2; RR2 = AUX; end
% es ist dann RR2 >= RR1
THETA1 = acos(RR2); % kleinerer Winkel
THETA2 = acos(RR1);
