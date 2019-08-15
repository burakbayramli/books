function F_WERT = bsp03(Y,flag,parmtr3);
% Sieben-Koerper-Problem  -------------------
% wie BSP03.M aber MODIFIZIERTE Zwangsbedingung fuer
% Newton-Verfahren in MOVIE
% flag = 1: Funktion g (Zwangsbwdingung)
% flag = 2: Gradient von g
% -------------------------------------------
% Gradient von f: R_n -> R_m ist (m,n)-Matrix
% -------------------------------------------
% Geometrische Daten -----------------------
d  = 0.028;    da = 0.0115;   e = 0.02;
ea = 0.01421;  zf = 0.02;    fa = 0.01421;
rr = 0.007;    ra = 0.00092; ss = 0.035;
sa = 0.01874;  sb = 0.01043; sc = 0.018;
sd = 0.02;     zt = 0.04;    ta = 0.02308;
tb = 0.00916;   u = 0.04;    ua = 0.01228;
ub = 0.00449;  c0 = 4530;    L0 = 0.07785;

X = [parmtr3,Y'];
sibe = sin(X(1)); cobe = cos(X(1));
sith = sin(X(2)); coth = cos(X(2));
siga = sin(X(3)); coga = cos(X(3));
siph = sin(X(4)); coph = cos(X(4));
side = sin(X(5)); code = cos(X(5));
siom = sin(X(6)); coom = cos(X(6));
siep = sin(X(7)); coep = cos(X(7));

sibeth = sin(X(1)+X(2)); cobeth = cos(X(1)+X(2));
siphde = sin(X(4)+X(5)); cophde = cos(X(4)+X(5));
siomep = sin(X(6)+X(7)); coomep = cos(X(6)+X(7));

FP = [-0.06934, -0.03635, 0.014;
      -0.00227,  0.03273, 0.072];
switch flag
case 1
   F_WERT = zeros(6,1);
   F_WERT(1) = rr*cobe - d*cobeth - ss*siga - FP(1,2);
   F_WERT(2) = rr*sibe - d*sibeth + ss*coga - FP(2,2);
   F_WERT(3) = rr*cobe - d*cobeth - e*siphde -  zt*code - FP(1,1);
   F_WERT(4) = rr*sibe - d*sibeth + e*cophde  - zt*side - FP(2,1);
   F_WERT(5) = rr*cobe - d*cobeth - zf*coomep - u*siep  - FP(1,1);
   F_WERT(6) = rr*sibe - d*sibeth - zf*siomep + u*coep  - FP(2,1);
case 2
   F_WERT = zeros(6,7);
   F_WERT(1,1) = -rr*sibe + d*sibeth;
   F_WERT(1,2) = d*sibeth;
   F_WERT(1,3) = -ss*coga;
   F_WERT(2,1) = rr*cobe- d*cobeth;
   F_WERT(2,2) = -d*cobeth;
   F_WERT(2,3) = -ss*siga;
   F_WERT(3,1) = -rr*sibe + d*sibeth;
   F_WERT(3,2) = d*sibeth;
   F_WERT(3,4) = -e*cophde;
   F_WERT(3,5) = -e*cophde + zt*side;
   F_WERT(4,1) = rr*cobe-d*cobeth;
   F_WERT(4,2) = -d*cobeth;
   F_WERT(4,4) = -e*siphde;
   F_WERT(4,5) = -e*siphde - zt*code;
   F_WERT(5,1) = -rr*sibe+ d*sibeth;
   F_WERT(5,2) = d*sibeth;
   F_WERT(5,6) = zf*siomep;
   F_WERT(5,7) = zf*siomep-u*coep;
   F_WERT(6,1) = rr*cobe - d*cobeth;
   F_WERT(6,2) = - d*cobeth;
   F_WERT(6,6) = -zf*coomep;
   F_WERT(6,7) = -zf*coomep - u*siep;
   F_WERT = F_WERT(:,2:7);
end
