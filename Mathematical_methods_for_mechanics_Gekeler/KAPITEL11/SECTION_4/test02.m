% Numerischer Kreisumfang
format long
AA = 2*pi
NN = 1000;
TT = linspace(0,2*pi,NN);
X = cos(TT); Y = sin(TT);
XX = X(2:NN) - X(1:NN-1);
XX = XX.*XX;
YY = Y(2:NN)- Y(1:NN-1);
YY = YY.*YY;
ZZ = XX + YY;
ZZ = sqrt(ZZ);
LL = sum(ZZ)

