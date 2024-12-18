% Hilfsfile 1 fuer DKT-ELEMENT
syms  x y;
% --- Berechnung von ME -----------------------------
N1  = '(1-x-y)*((1-x+2*y)*(1+2*x-y)-16*x*y)';
N2  = 'x*(1-x-2*y)*(1-x-y)';
N3  = 'y*(1-2*x-y)*(1-x-y)';
N4  = 'x*x*(3-2*x)-7*x*y*(1-x-y)';
N5  = 'x*x*(x-1) + 2*x*y*(1-x-y)';
N6  = '-x*y*(1-2*x-y)';
N7  = 'y*y*(3-2*y)-7*x*y*(1-x-y)';
N8  = ' -x*y*(1-x-2*y)';
N9  = 'y*y*(y-1)+2*x*y*(1-x-y)';
% -------------------------------
f1  = symmul(N1,N1);
f2  = int(f1,'x',0,1-y);
M11 = 10080*int(f2,'y',0,1)
f1  = symmul(N1,N2);
f2  = int(f1,'x',0,1-y);
M12 = 10080*int(f2,'y',0,1)

f1  = symmul(N1,N3);
f2  = int(f1,'x',0,1-y);
M13 = 10080*int(f2,'y',0,1)
f1  = symmul(N1,N4);
f2  = int(f1,'x',0,1-y);
M14 = 10080*int(f2,'y',0,1)
f1  = symmul(N1,N5);
f2  = int(f1,'x',0,1-y);
M15 = 10080*int(f2,'y',0,1)
f1  = symmul(N1,N6);
f2  = int(f1,'x',0,1-y);
M16 = 10080*int(f2,'y',0,1)
f1  = symmul(N1,N7);
f2  = int(f1,'x',0,1-y);
M17 = 10080*int(f2,'y',0,1)
f1  = symmul(N1,N8);
f2  = int(f1,'x',0,1-y);
M18 = 10080*int(f2,'y',0,1)
f1  = symmul(N1,N9);
f2  = int(f1,'x',0,1-y);
M19 = 10080*int(f2,'y',0,1)
% -----------------------
f1  = symmul(N2,N2);
f2  = int(f1,'x',0,1-y);
M22 = 10080*int(f2,'y',0,1)
f1  = symmul(N2,N3);
f2  = int(f1,'x',0,1-y);
M23 = 10080*int(f2,'y',0,1)
f1  = symmul(N2,N4);
f2  = int(f1,'x',0,1-y);
M24 = 10080*int(f2,'y',0,1)
f1  = symmul(N2,N5);
f2  = int(f1,'x',0,1-y);
M25 = 10080*int(f2,'y',0,1)
f1  = symmul(N2,N6);
f2  = int(f1,'x',0,1-y);
M26 = 10080*int(f2,'y',0,1)
f1  = symmul(N2,N7);
f2  = int(f1,'x',0,1-y);
M27 = 10080*int(f2,'y',0,1)
f1  = symmul(N2,N8);
f2  = int(f1,'x',0,1-y);
M28 = 10080*int(f2,'y',0,1)
f1  = symmul(N2,N9);
f2  = int(f1,'x',0,1-y);
M29 = 10080*int(f2,'y',0,1)
% --------------------------
f1  = symmul(N3,N3);
f2  = int(f1,'x',0,1-y);
M33 = 10080*int(f2,'y',0,1)
f1  = symmul(N3,N4);
f2  = int(f1,'x',0,1-y);
M34 = 10080*int(f2,'y',0,1)
f1  = symmul(N3,N5);
f2  = int(f1,'x',0,1-y);
M35 = 10080*int(f2,'y',0,1)
f1  = symmul(N3,N6);
f2  = int(f1,'x',0,1-y);
M36 = 10080*int(f2,'y',0,1)
f1  = symmul(N3,N7);
f2  = int(f1,'x',0,1-y);
M37 = 10080*int(f2,'y',0,1)
f1  = symmul(N3,N8);
f2  = int(f1,'x',0,1-y);
M38 = 10080*int(f2,'y',0,1)
f1  = symmul(N3,N9);
f2  = int(f1,'x',0,1-y);
M39 = 10080*int(f2,'y',0,1)
% --------------------------
f1  = symmul(N4,N4);
f2  = int(f1,'x',0,1-y);
M44 = 10080*int(f2,'y',0,1)
f1  = symmul(N4,N5);
f2  = int(f1,'x',0,1-y);
M45 = 10080*int(f2,'y',0,1)
f1  = symmul(N4,N6);
f2  = int(f1,'x',0,1-y);
M46 = 10080*int(f2,'y',0,1)
f1  = symmul(N4,N7);
f2  = int(f1,'x',0,1-y);
M47 = 10080*int(f2,'y',0,1)
f1  = symmul(N4,N8);
f2  = int(f1,'x',0,1-y);
M48 = 10080*int(f2,'y',0,1)
f1  = symmul(N4,N9);
f2  = int(f1,'x',0,1-y);
M49 = 10080*int(f2,'y',0,1)
% --------------------------
f1  = symmul(N5,N5);
f2  = int(f1,'x',0,1-y);
M55 = 10080*int(f2,'y',0,1)
f1  = symmul(N5,N6);
f2  = int(f1,'x',0,1-y);
M56 = 10080*int(f2,'y',0,1)
f1  = symmul(N5,N7);
f2  = int(f1,'x',0,1-y);
M57 = 10080*int(f2,'y',0,1)
f1  = symmul(N5,N8);
f2  = int(f1,'x',0,1-y);
M58 = 10080*int(f2,'y',0,1)
f1  = symmul(N5,N9);
f2  = int(f1,'x',0,1-y);
M59 = 10080*int(f2,'y',0,1)
% --------------------------
f1  = symmul(N6,N6);
f2  = int(f1,'x',0,1-y);
M66 = 10080*int(f2,'y',0,1)
f1  = symmul(N6,N7);
f2  = int(f1,'x',0,1-y);
M67 = 10080*int(f2,'y',0,1)
f1  = symmul(N6,N8);
f2  = int(f1,'x',0,1-y);
M68 = 10080*int(f2,'y',0,1)
f1  = symmul(N6,N9);
f2  = int(f1,'x',0,1-y);
M69 = 10080*int(f2,'y',0,1)
% --------------------------
f1  = symmul(N7,N7);
f2  = int(f1,'x',0,1-y);
M77 = 10080*int(f2,'y',0,1)
f1  = symmul(N7,N8);
f2  = int(f1,'x',0,1-y);
M78 = 10080*int(f2,'y',0,1)
f1  = symmul(N7,N9);
f2  = int(f1,'x',0,1-y);
M79 = 10080*int(f2,'y',0,1)
% --------------------------
f1  = symmul(N8,N8);
f2  = int(f1,'x',0,1-y);
M88 = 10080*int(f2,'y',0,1)
f1  = symmul(N8,N9);
f2  = int(f1,'x',0,1-y);
M89 = 10080*int(f2,'y',0,1)
% --------------------------
f1  = symmul(N9,N9);
f2  = int(f1,'x',0,1-y);
M99 = 10080*int(f2,'y',0,1)
% -------------------------
