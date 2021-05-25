function effDiv = cfdComputeEffectiveDivergence(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function claculates effective divergence
%--------------------------------------------------------------------------

if nargin==0
    theEquationName = 'U';
else
    theEquationName = varargin{1};
end

% Get info
owners = cfdGetOwnersSubArrayForFaces;
neighbours = cfdGetNeighboursSubArrayForFaces;
theNumberofInteriorFaces = cfdGetNumberOfInteriorFaces;
theNumberOfFaces = cfdGetNumberOfFaces;
theNumberOfElements = cfdGetNumberOfElements;

% Get the mdot_f field. Multiply by specific heat if the equation is the
% energy equation
mdot_f = cfdGetDataArray('mdot_f');
if strcmp(theEquationName, 'T')
    Cp = cfdGetDataArray('Cp');
    Cp_f = cfdInterpolateFromElementsToFaces('linear', Cp);
    mdot_f = mdot_f .* Cp_f;
end

% Initialize effective divergence array
effDiv = cfdScalarList(theNumberOfElements);

% Interior Faces Contribution
for iFace=1:theNumberofInteriorFaces
    own = owners(iFace);
    nei = neighbours(iFace);
    
    effDiv(own) = effDiv(own) + mdot_f(iFace);
    effDiv(nei) = effDiv(nei) - mdot_f(iFace);
end

% Boundary Faces Contribution
for iBFace=theNumberofInteriorFaces+1:theNumberOfFaces
    own = owners(iBFace);
    effDiv(own) = effDiv(own) + mdot_f(iBFace);
end
