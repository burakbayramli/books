%   Blade Element Momentum Theory analysis of a Single Rotor Helicopter
%   In Hover
%
%	Stephen Bell
%   November 4, 2008
%   stephen.bell@ymail.com
%
%   This is based on the BEMT theory as described by Dr. J.Gordon Leishman
%   in his book "Principles of Helicopter Aerodynamics
%
%
%   Purpose:    The aim of this code is to calculate the thrust and power
%               characteristics of a single rotor helicopter in hovering 
%               flight. This is done using Blade Element Momentum Theory
%               to calculate the inflow, thrust and power for each blade 
%               segment then sum them along the length of the blade.
%
%               This kind of analysis is useful for a preliminary rotary
%               winged vehicle, and has been used as the bases for an   
%               optimisation for vehicle design.
%
%   Inputs: -Applied Angle of Incidence of the blade (Angle between the
%           chordline and the horizontal) (deg)
%
%           -RPM of the rotor
%
%           -BladeCharacteristics Structure containing:
%               -BChar.Cla  = Cl/Alpha slope of airfoil (1/deg)
%               -BChar.Cd   = Drag Coefficient of blade at Angle of
%                             Incidence
%               -BChar.Nb   = Number of Blades
%               -BChar.c    = Blade Chord Length  (m)
%               -BChar.Rmax = Max Radius of Blade (m)
%               -BChar.Rmin = Root Cutout Length (m) 
%
%           -Number of Radial stations for calculation (n=100 is typical)
%
%
%   Outputs: [3x1 Vector, 2x1 Cell]
%               -[Thrust Generated, Power Required, Figure of Merit]
%               -[Radial Locations Vector, Non-Dimensional Inflow Vector]
%
%
%
%


function [ret,vect] = BEMTsingle(alpha, rpm, BChar, n)
    
    
 
    global Cla sigma AoA rev dr Nb r

    %INPUTS
    Cla = BChar.Cla;
    Cd = BChar.Cd;          %Drag Coefficient at AoA
    rho = 1.225;            %Density of Air
    Nb = BChar.Nb;          %Number of blades per rotor
    c = BChar.c;            %Blade Chord
    AoA = alpha;            %Angle of attack in degrees
    RPM = rpm;              %RPM
    Rmax = BChar.Rmax;      %Maximum Rotor Radius
    Rmin = BChar.Rmin;      %Root Cut out 
    Nr = n;                 %Number of Radial Stations 
                            %for calculation

    %CALCULATED VALUES

    TipDeflection = 0.08*Cla*AoA;
    NormalR = sqrt((Rmax-Rmin)^2-TipDeflection^2);
    sigma = (Nb*c)/(pi*(Rmax));     %Solidity per rotor
    rev = RPM*(2*pi)/60;            %Radians per second
    dr = 1/Nr;                      %Radial incriment 
                                    %percentage
    r = (dr:dr:1);                  %preallocating the r-range
    Cla = Cla*180/pi;               %Converting 1/deg to 1/rad
    AoA = AoA*pi/180;               %Converting deg to rad
    vz = 0;                         %Nondimensionalised vertical V (zero at the moment)
    Ad = pi*((Rmax-Rmin)^2);        %Disc Area
 
    laminf = vz*(ones(1,length(r)));
    lam = lam_calc(laminf);


    %Calculation of the Coefficients
    dCt_on_dr = dCt(lam);
    Ct = sum(dr*dCt_on_dr);  
    dCpo = (sigma*Cd*dr/2)*(r.^3);
    dCpi = dr*(dCt_on_dr.*lam); 
    Cp = sum(dCpo + dCpi);
    
    Thrust = (NormalR/(Rmax-Rmin))*Ct*rho*Ad*((rev*Rmax)^2);
    Power = (NormalR/(Rmax-Rmin))*Cp*rho*Ad*((rev*Rmax)^3);
    
    FoM = (sum(dCpi))/(((Ct)^(3/2))/sqrt(2));

    
    ret = [Thrust,Power,FoM];
    vect = {r,lam};
end


function out = lam_calc(Laminf)
    global Cla sigma AoA r
    out = zeros(1,length(r));   %Preallocating  and initialising
                                %the out vector
    i = 1;
    while i <= 5                        
        A = (sigma*Cla/16)./(F(out)) - Laminf/2;
        B = (sigma*Cla*AoA/8)*(r./F(out));
    
        out = sqrt(A.^2 + B) - A;
        i = i + 1;
    end

    return
end

function out = F(v)
    global Nb r
    f = (Nb/2)*((1-r)./v);
    out = (2/pi)*acos(exp(-f));
    out(length(out)) = out(length(out)-1)/2;
end

function out = dCt(lami)
    global r sigma Cla AoA
    out = (sigma*Cla/2)*(AoA*(r.^2) - lami.*r);
    
end

    
