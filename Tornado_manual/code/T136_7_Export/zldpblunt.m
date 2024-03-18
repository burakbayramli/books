function[CD0]=zldpblunt(Mach,Alt,body,ref)
%
%
%zldpblund, Zero Lift Drag Prediction of blunt bodies.
%This function predicts the zero lift drag of the blunt parts of an airplane configuration
%
%Component buildup method, Eckerts equation, Raymer formulatation, Melin
%Implementation
%
%--------------------------------------------
%
%Inputs are:
% Tornado variable structures.
% Mach: Mach number, useful range [0 0.7], it will still work up to M=1,
%       but the errors will start to increase
%
% Alt:  Altitude in meters. The atmosphere model used is the 1976 
%       International Standard Atmosphere (ISA).
%
% body:  Geometry structure similar  to the onedefined in Tornado. It should contain
%
%       body.length     Lengths of blunt bodies such as:  [Body  Nacelle Nacelle]
%       body.diam       Diameter of blunt bodies in the same order as the
%                       lengths
%       body.inter      Body interference factors
%
% ref:  Reference units structure as defined in Tornado.
%
%----------------------------------------------
%
% Outputs:
%
% CD0:  Vector with zero lift drag component of each body.
%   
%
%-----------------------------------------------
%
% LIMITATIONS
% 
% Tubulent transition is hardcoded to 0% chord.
% Surface roughness is hardcoded to smooth paint.
% First element is assumed to be the fuselage, all other to be nacelles etc...
%
%
% Revision history:
%   Spånga, 2021-09-19:   Updated to MATLAB R2020, TM  
%% Constants definition

A=0.455;    
b=2.58;
c=0.144;
d=0.65;

e=1.053;
k=0.634*10^(-5);                   %HARDCODED, Smooth paint (Raymer table 12.4)

f=1.328;

ts=size(body.length);

%% Computing state
[rho a p mu]=ISAtmosphere(Alt);     %Calling International Standard atmosphere.
V=Mach*a;                          %Computing local TAS 

%% Friction coefficient
body.Re=(rho*V*body.length)./(mu);                             %Reynolds Number for bodies
body.Re_cutoff=38.21*(body.length/k).^e;                       %Cutoff Re

for t=1:ts
        body.Re_use(t)=min([body.Re(t) body.Re_cutoff(t)]);    %Use smallest value
end

body.cf=A./(log10(body.Re_use).^b*(1+c*Mach^2)^d); %body turbulent skin friction coefficient;

%% Form Factor
f=body.length./body.diam;

body.FF(1)=(1+60./(f(1).^3)+f(1)./400); %Fuselage Formfactor
body.FF=[body.FF 1+(0.35./f(2:end))];   %Nacelle formfactor

%% Wetted area
Swet=pi*body.diam.*body.length;        % Analytic, not useful
Swet=Swet*0.9;                              % Fudging for nose and tailcone, BLACK MAGIC NUMBER

%% Interference Factors
QQ=body.inter;

%% Drag coefficients
CD0=body.cf.*body.FF.*QQ.*Swet./ref.S_ref;
