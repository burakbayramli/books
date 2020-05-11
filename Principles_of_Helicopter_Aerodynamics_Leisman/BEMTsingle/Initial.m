
RPM = 1000;         %Rev / Min

alpha = 4;          %Deg

BChar.Cla = 0.11;   %Slope of the Cl v Alpha linear approximation 
                    %for the airfoil being used (eg NACA 0012)
BChar.Cd = 0.4;     %Cd for the airfoil for given Alpha
BChar.Nb = 2;       %Number of blades
BChar.c = 0.040;    %Blade Chord Length (Note can't accept variable chord)
BChar.Rmax = 1.0;   %Max Radius (m)
BChar.Rmin = 0.01;  %Root cutout length (Accounts for housing interupting
                    %airflow at root)
                    
n = 100;            %Number of radial stations to calculate

[out1,out2] = BEMTsingle(alpha, RPM, BChar, n)
