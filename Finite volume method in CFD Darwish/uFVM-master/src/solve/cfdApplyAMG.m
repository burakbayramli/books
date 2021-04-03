function [initialResidual, finalResidual] = cfdApplyAMG(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%  Solve using Geometric-Algebraic multi-grid solver
%--------------------------------------------------------------------------

% Explode settings
if isempty(varargin)
    preconditioner = 'DILU';
    maxIter = 20;
    tolerance = 1e-6;
    relTol = 0.1;
    nPreSweeps = 0;
    nPostSweeps = 2;
    nFinestSweeps = 2;
elseif nargin==1
    preconditioner = varargin{1};
    maxIter = 20;
    tolerance = 1e-6;
    relTol = 0.1;
    nPreSweeps = 0;
    nPostSweeps = 2;
    nFinestSweeps = 2;  
elseif nargin==2
    preconditioner = varargin{1};
    maxIter = varargin{2};
    tolerance = 1e-6;
    relTol = 0.1;
    nPreSweeps = 0;
    nPostSweeps = 2;
    nFinestSweeps = 2;  
elseif nargin==3
    preconditioner = varargin{1};
    maxIter = varargin{2};
    tolerance = varargin{3};
    relTol = 0.1;
    nPreSweeps = 0;
    nPostSweeps = 2;
    nFinestSweeps = 2;  
elseif nargin==4
    preconditioner = varargin{1};
    maxIter = varargin{2};
    tolerance = varargin{3};
    relTol = varargin{4};
    nPreSweeps = 0;
    nPostSweeps = 2;
    nFinestSweeps = 2;  
elseif nargin==5
    preconditioner = varargin{1};
    maxIter = varargin{2};
    tolerance = varargin{3};
    relTol = varargin{4};
    nPreSweeps = varargin{5};
    nPostSweeps = 2;
    nFinestSweeps = 2;  
elseif nargin==6
    preconditioner = varargin{1};
    maxIter = varargin{2};
    tolerance = varargin{3};
    relTol = varargin{4};
    nPreSweeps = varargin{5};
    nPostSweeps = varargin{6};
    nFinestSweeps = 2;  
elseif nargin==7
    preconditioner = varargin{1};
    maxIter = varargin{2};
    tolerance = varargin{3};
    relTol = varargin{4};
    nPreSweeps = varargin{5};
    nPostSweeps = varargin{6};
    nFinestSweeps = varargin{7};
end

% Default settings
cycleType = 'V-Cycle';
maxCoarseLevels = 10;


% Build course grids
maxLevels = cfdAgglomerate(maxCoarseLevels);

% Calculate initial residual
theCoefficients = cfdGetCoefficients;
residualsArray = cfdComputeResidualsArray(theCoefficients);
initialResidual = sum(abs(residualsArray));
finalResidual = initialResidual;

% Cycle
if maxLevels<=3    
    for iter=1:maxIter
        finalResidual = cfdApplyVCycle(1,preconditioner,maxLevels,nPreSweeps,nPostSweeps,relTol,nFinestSweeps);
        if (finalResidual<relTol*initialResidual) && (finalResidual<tolerance)
            break;
        end            
    end
    return;
end

if strcmp(cycleType,'V-Cycle')
    for iter=1:maxIter
        finalResidual = cfdApplyVCycle(1,preconditioner,maxLevels,nPreSweeps,nPostSweeps,relTol,nFinestSweeps);        
        if (finalResidual<relTol*initialResidual) && (finalResidual<tolerance)
            break;
        end 
    end
end