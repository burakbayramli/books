function Up = akzonobelf(t, U)

  k1  = 18.7;
  k2  = 0.58;
  k3  = 0.09;
  k4  = 0.42;
  K   = 34.4;
  klA = 3.3;
  p   = 0.9;
  H   = 737;
  
  r1 = k1*U(1)^4*sqrt(U(2));
  r2 = k2*U(3)*U(4);
  r3 = (k2/K)*U(1)*U(5);
  r4 = k3*U(1)*U(4)^2;
  r5 = k4*U(6)^2*sqrt(U(2));

  F = klA * (p/H - U(2));
  
  Up(1) = -2*r1 + r2 - r3 - r4;
  Up(2) = -0.5*r1 - r4 - 0.5*r5 + F;
  Up(3) = r1 - r2 + r3;
  Up(4) = -r2 + r3 - 2*r4;
  Up(5) = r2 - r3 + r5;
  Up(6) = -r5;
