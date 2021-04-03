function [initialResidual, finalResidual] = cfdSolveAlgebraicSystem(gridLevel,varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------


% Explode settings
if isempty(varargin)
    smoother = 'DILU';
    maxIter = 20;
    tolerance = 1e-6;
    relTol = 0.1;
elseif nargin==2
    smoother = varargin{1};
    maxIter = 20;
    tolerance = 1e-6;
    relTol = 0.1; 
elseif nargin==3
    smoother = varargin{1};
    maxIter = varargin{2};
    tolerance = 1e-6;
    relTol = 0.1;
elseif nargin==4
    smoother = varargin{1};
    maxIter = varargin{2};
    tolerance = varargin{3};
    relTol = 0.1; 
elseif nargin==5
    smoother = varargin{1};
    maxIter = varargin{2};
    tolerance = varargin{3};
    relTol = varargin{4};
end


theCoefficients = cfdGetCoefficients(gridLevel);
ac = theCoefficients.ac;
anb = theCoefficients.anb;
bc = theCoefficients.bc;
cconn = theCoefficients.cconn;
dphi = theCoefficients.dphi;
theNumberOfElements = theCoefficients.numberOfElements;

% Compute initial residual
residualsArray = cfdComputeResidualsArray(theCoefficients);
initialResidual = sum(abs(residualsArray))/theNumberOfElements;
finalResidual = initialResidual;

if maxIter==0
    return;
end

if strcmp(smoother,'DILU')
    
    % Factorize Ax=b (Apply incomplete upper lower decomposition)
    [dc, rc] = cfdFactorizeILU(ac,anb,bc,cconn);
    
    % Solve system
    for iter=1:maxIter
        dphi = cfdSolveILU(ac,anb,bc,dc,rc,cconn,dphi);
        theCoefficients.dphi = dphi;
        
        % Check if termination criterion satisfied
        residualsArray = cfdComputeResidualsArray(theCoefficients);
        finalResidual = sum(abs(residualsArray))/theNumberOfElements;
        
        if (finalResidual<relTol*initialResidual) && (finalResidual<tolerance)
            break;
        end        
    end
elseif strcmp(smoother,'SOR') || strcmp(smoother,'GaussSeidel')
    
    % Solve system
    for iter=1:maxIter
        dphi = cfdSolveSOR(ac,anb,bc,cconn,dphi);
        theCoefficients.dphi = dphi;
        
        % Check if termination criterion satisfied
        residualsArray = cfdComputeResidualsArray(theCoefficients);
        finalResidual = sum(abs(residualsArray))/theNumberOfElements;
        
        if (finalResidual<relTol*initialResidual) && (finalResidual<tolerance)
            break;
        end 
    end
end

% Store
theCoefficients.dphi = dphi;
cfdSetCoefficients(theCoefficients, gridLevel);