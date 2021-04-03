function coefficients3 = cfdSetupCoefficients3(theCConn,theCSize)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function set up the coefficients
%--------------------------------------------------------------------------

theNumberOfElements = length(theCSize);

ac = zeros(theNumberOfElements,1);
ac_old = zeros(theNumberOfElements,1);
bc = zeros(theNumberOfElements,1);

% dc & rc added to be used in ILU Solver
dc = zeros(theNumberOfElements,1);
rc = zeros(theNumberOfElements,1);

dphi = zeros(theNumberOfElements,1);

anb = cell(theNumberOfElements,1);

for iElement=1:theNumberOfElements
    anb{iElement} = zeros(1,theCSize(iElement));
end

coefficients3.ac = ac;
coefficients3.ac_old = ac_old;
coefficients3.bc = bc;
coefficients3.anb = anb;

%dc & rc added to be used in ILU Solver
coefficients3.dc = dc;
coefficients3.rc = rc;

coefficients3.dphi = dphi;

coefficients3.cconn = theCConn;
coefficients3.csize = theCSize;

coefficients3.numberOfElements = theNumberOfElements;
