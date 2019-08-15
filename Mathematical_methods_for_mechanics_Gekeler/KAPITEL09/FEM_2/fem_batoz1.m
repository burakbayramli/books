function [B,DET,ecode] = batoz1(X,Y,XI,ETA)
% GEKELER: FINITE ELEMENTE -------------------------
% LIEFERT den VEktor B = des DKT-Elementes
% H1 = HX_XI,HY_XI, HX_ETA, HY_ETA]
% KUBISCHER ANSATZ NACH BATOZ ET AL., PLATTENPROBLEME
% INPUT:
%      XK,YK: die drei Eckenkoordinatenpaare des Dreiecks in der
%             ueblichen Reihenfolge

ecode = 0;
X12   = X(1) - X(2); X23   = X(2) - X(3); X31   = X(3) - X(1);
Y12   = Y(1) - Y(2); Y23   = Y(2) - Y(3); Y31   = Y(3) - Y(1);
LL12  = X12*X12 + Y12*Y12; LL23  = X23*X23 + Y23*Y23;
LL31  = X31*X31 + Y31*Y31;
L12   = sqrt(LL12); L23   = sqrt(LL23);
L31   = sqrt(LL31);
A     = - [X23/LL23, X31/LL31, X12/LL12];
B     =   3*[X23*Y23/LL23, X31*Y31/LL31, X12*Y12/LL12]/4;
C     =   [(2*X23*X23 - Y23*Y23)/LL23, (2*X31*X31 - Y31*Y31)/LL31,...
          (2*X12*X12 - Y12*Y12)/LL12]/4;
D     = - [Y23/LL23, Y31/LL31, Y12/LL12];
E     =   [(2*Y23*Y23 - X23*X23)/LL23, (2*Y31*Y31 - X31*X31)/LL31,...
          (2*Y12*Y12 - X12*X12)/LL12]/4;
DET   =   X31*Y12 - X12*Y31;
if DET <= 0
   ecode = 1;
end
P     = 6*A;
T     = 6*D;
Q     = 4*B;
R     = 3*[Y23*Y23/LL23, Y31*Y31/LL31, Y12*Y12/LL12];

HX_XI = [...
  P(3)*(1 - 2*XI) + (P(2) - P(3))*ETA;
  Q(3)*(1 - 2*XI) - (Q(2) + Q(3))*ETA;
- 4 + 6*(XI + ETA) + R(3)*(1 - 2*XI) - ETA*(R(2) + R(3));
- P(3)*(1 - 2*XI) + ETA*(P(1) + P(3));
  Q(3)*(1 - 2*XI) - ETA*(Q(3) - Q(1));
- 2 + 6*XI + R(3)*(1 - 2*XI) + ETA*(R(1) - R(3));
- ETA*(P(2) + P(1));
  ETA*(Q(1) - Q(2));
- ETA*(R(2) - R(1))];

HY_XI = [...
  T(3)*(1 - 2*XI) + ETA*(T(2) - T(3));
  1 + R(3)*(1 - 2*XI) - ETA*(R(2) + R(3));
- Q(3)*(1 - 2*XI) + ETA*(Q(2) + Q(3));
- T(3)*(1 - 2*XI) + ETA*(T(1) + T(3));
- 1 + R(3)*(1 - 2*XI) + ETA*(R(1) - R(3));
- Q(3)*(1 - 2*XI) - ETA*(Q(1) - Q(3));
- ETA*(T(1) + T(2));
  ETA*(R(1) - R(2));
- ETA*(Q(1) - Q(2))];

HX_ETA = [...
- P(2)*(1 - 2*ETA) - XI*(P(3) - P(2));
  Q(2)*(1 - 2*ETA) - XI*(Q(2) + Q(3));
- 4 + 6*(XI + ETA) + R(2)*(1 - 2*ETA) - XI*(R(2) + R(3));
  XI*(P(1) + P(3));
  XI*(Q(1) - Q(3));
- XI*(R(3) - R(1));
  P(2)*(1 - 2*ETA) - XI*(P(1) + P(2));
  Q(2)*(1 - 2*ETA) + XI*(Q(1) - Q(2));
- 2 + 6*ETA + R(2)*(1 - 2*ETA) + XI*(R(1) - R(2))];

HY_ETA = [...
- T(2)*(1 - 2*ETA) - XI*(T(3) - T(2));
  1 + R(2)*(1 - 2*ETA) - XI*(R(2) + R(3));
- Q(2)*(1 - 2*ETA) + XI*(Q(2) + Q(3));
  XI*(T(1) + T(3));
  XI*(R(1) - R(3));
- XI*(Q(1) - Q(3));
  T(2)*(1 - 2*ETA) - XI*(T(1) + T(2));
- 1 + R(2)*(1 - 2*ETA) + XI*(R(1) - R(2));
- Q(2)*(1 - 2*ETA) - XI*(Q(1) - Q(2))];

B = [...
  Y31*HX_XI + Y12*HX_ETA, - X31*HY_XI - X12*HY_ETA, ...
- X31*HX_XI - X12*HX_ETA + Y31*HY_XI + Y12*HY_ETA];
