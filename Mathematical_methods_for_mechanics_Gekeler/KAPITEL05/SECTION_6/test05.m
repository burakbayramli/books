function test05
NU = 2;  % to be chosen !!!!!!!!!!
n   = 18;    % Number of discretization points on X- resp. Y-axis
SIGN = 1;
Parmeter  = [0;NU;SIGN;n]; 
U = bsp05a(0,4,Parmeter);
save daten U Parmeter
z1 = fminbnd(@test07,0,0.5);
%z2 = fminbnd(@test07,0.5,1)
zeta = [z1;sqrt(1 - z1^2)];
U0 = U*zeta;
DIFF = U.'*U0 - U.'*U0.^3

UU = U.'*U

function Y = test07(x);

load daten U Parmeter
SIGN = Parmeter(3);
y = sqrt(1 - x^2);
X = U*[x;y];
Y = sum(X.^4)/4;

    