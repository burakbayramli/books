% myCon   Defines useful constants in the workspace
%         This must be a script, not a function m-file, in order to add
%         variables defined here to the workspace

% --- fundamental constants
Avogadro = 6.024e23;      %  number of molecules in a mole
cLight = 2.998e8;         %  Speed of light;  m/s
gravity = 9.807;          %  acceleration due to gravity,  m/s^2
hPlanck = 6.625e-34;      %  Planck's "h" constant;  J*s/molecule
kBoltzmann = 1.380e-23;   %  Boltzmann's constant;  J/K/molecule
Rgas = 8315;              %  Universal gas constant;  J/kmol/K

% --- unit conversions
galPerMeter3 = 264.17;            %  number of US gallons in a cubic meter
galPerLiter = galPerMeter3/1000;  %  number of US gallons in a liter
inchPerMeter = 1/2.54e-2;         %  number of inches in a meter
inchPerGal = 231;                 %  number of cubic inches in a gallon
lbPerkg = 2.2046;                 %  number of pounds (mass) per kilogram
psiPerPascal = 14.696/101325;     %  number of PSI per Pascal
