% Function to solve a determined trim system with one or two trim variables.
% Angle of attack (AoA) is the first trim variable.  Optionally, an 
% elevator trim variable can be specified in the input file.   
%
% Anthony Ricciardi
% August 2017
% 
% Inputs: 
% FEM = [struct] model data
%
% trimID = [int] ID of TRIM input entry that specifies the trim conditions
%          and trim varaibles.  
%
% w = Matrix of influnce coefficients.  Flow through each panel induced
%     proportionally by each vortex [number of panels x number of panels]
%
% Vind14 = Velocity at the 1/4 chord of each panel induced proportionally 
%          by each vortex [number of panels x number of panels x 3]
%
%
% Outputs: 
% C0 -> [struct] Force and moment coefficents
%   .L = Lift coefficient
%   .D = Drag coefficient
%   .M = Pitch moment coefficient (reference point specified in AEROZ entry)
%   .N = Normal force coefficient
%
% Dim -> [struct] Force and moment coefficents
%    .F = [num panels x 3] Forces at panel quarter chord control point
%    .bodyF = [1x3] total-aicraft body forces
%
% FEM = [struct] model data (modified)
% iter = [int] number of iterations used in trim solve
% fun = [num trim vars x 1] Newton-Raphson function residuals
% alpha = Trim angle of attack - degrees
% delta = Trim control surface deflection - degreees
%
function [C0,Dim,FEM,iter,fun,alpha,delta] = VoLAre_trim(FEM,trimID,w,Vind14)

%% Symmetric Factor
switch FEM.AEROZ.XZSYM
    case 'YES'
        symmetricFactor = 2;
        symmetricFactor_M = [0,2,0];
    case 'NO'
        symmetricFactor = 1;
        symmetricFactor_M = [1,1,1];
    otherwise
        error('FEM.AEROZ.XZSYM option not supported.')
end

%% Manage Trim Inputs
if isfield(FEM,'TRIM') == 0
    error('No TRIM entry.  It''s required.')
end
trimIndex = find([FEM.TRIM.TRIMID]==trimID);
% overwrite with single TRIM
FEM.TRIM=FEM.TRIM(trimIndex);

%% Figure Out Trim Varaiables
bodyF_target = FEM.TRIM.WEIGHT*FEM.TRIM.NZ;
CN_target = bodyF_target/(FEM.TRIM.QINF *FEM.AEROZ.REFS);

if FEM.TRIM.QDOT ~= 0
    error('VoLAre_trim only set for TRIM QDOT = 0.  Modify TRIM input or code.')
end
CM_target = 0.0;

%% Magange trim variables
if isfield(FEM,'TRIMVAR') == 0
    error('No TRIMVAR entry.  It''s required.')
end
n_trimVars = size(FEM.TRIMVAR,2);
if ~(n_trimVars == 1 || n_trimVars == 2)
    error('Trim only set up for 1 or 2 trim variables.  Update the code or the input file.')
end

TRIMVAR_IDs = zeros(n_trimVars,1);
alphaID = [];
deltaID = [];
for i = 1:size(FEM.TRIMVAR,2)
    TRIMVAR_IDs(i) = FEM.TRIMVAR(i).IDVAR;
    if strcmp(FEM.TRIMVAR(i).LABEL,'ALPHA')
        if ~isempty(alphaID)
            error('only one TRIMVAR LABEL ALPHA is allowed')
        end
        alphaID = i;
    elseif strcmp(FEM.TRIMVAR(i).LABEL,'ELEVATOR')
        if ~isempty(deltaID)
            error('only one TRIMVAR LABEL ELEVATOR is allowed')
        end
        deltaID = i;
    else
        error('You have a TRIMVAR LABEL set to %s. Only TRIMVAR LABELs ALPHA and ELEVATOR are supported',FEM.TRIMVAR(i).LABEL)
    end
end

if isempty(alphaID)
    error('One and only one TRIMVAR LABEL must be ALPHA')
end


%% Run Trim Analysis
% initialize
fd = 1e-9; % finite difference step size
fun = 10;
funTol = 1e-9;
iter = 0;
maxIter = 100;

% set initial alpha
alpha = FEM.TRIMVAR(alphaID).INITIAL;

switch n_trimVars
    case 1
        C0 =   VoLAre_solve(FEM,w,Vind14,alpha);
        while max(abs(fun)) > funTol && iter < maxIter
            iter = iter + 1;
            Cda  = VoLAre_solve(FEM,w,Vind14,alpha+fd);
            
            Jac = (Cda.N(3)-C0.N(3))/fd;
            
            fun = C0.N(3)-CN_target;
            alpha = alpha - fun/Jac;
            
            [C0,Dim,FEM] =   VoLAre_solve(FEM,w,Vind14,alpha);
            fun = C0.N(3)-CN_target;
        end
		
		
    case 2
        % set initial delta
        delta = FEM.TRIMVAR(deltaID).INITIAL;
		
        C0 =   VoLAre_solve(FEM,w,Vind14,alpha,delta);
        while max(abs(fun)) > funTol && iter < maxIter
            iter = iter + 1;
            Cda  = VoLAre_solve(FEM,w,Vind14,alpha+fd,delta);
            Cdd  = VoLAre_solve(FEM,w,Vind14,alpha   ,delta+fd);
            
            Jac = [(Cda.N(3)-C0.N(3))/fd     (Cdd.N(3)-C0.N(3))/fd;
                (Cda.M(2)-C0.M(2))/fd  (Cdd.M(2)-C0.M(2))/fd];
            
            fun = [C0.N(3)-CN_target; C0.M(2)-CM_target];
            alphaDeltaNew = [alpha; delta] - Jac\fun;
            
            alpha = alphaDeltaNew(1);
            delta = alphaDeltaNew(2);
            
            [C0,Dim,FEM] = VoLAre_solve(FEM,w,Vind14,alpha,delta);
            fun = [C0.N(3)-CN_target;  C0.M(2)-CM_target];
        end
    otherwise
        error('Too many trim variables')
end

%%
if iter == maxIter
	error('Maximum iterations exceeded in trim solve.')
end