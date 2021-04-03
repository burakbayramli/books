function cfdComputeScaledRMSResiduals(theEquationName, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function computes scaled residuals
%--------------------------------------------------------------------------

if nargin==1
    iComponent = 1;
end

% Get scale
scale = cfdGetFieldScale(theEquationName);

% Get coefficients
theCoefficients = cfdGetCoefficients;
ac = theCoefficients.ac;
bc = theCoefficients.bc;

if strcmp(theEquationName,'p')
    % Fore pressure correction equation, the divergence of the mass flow
    % rate is the residual.  
            
    % Scale with scale value (max value)
    p_scale = cfdGetFieldScale('p');
    maxScaledResidual = max(abs(bc)./(ac*p_scale));
    rmsScaledResidual = rms(abs(bc)./(ac*p_scale));             
else 
    % Other equations ...    
    
    % Get info
    theNumberOfElements = cfdGetNumberOfElements;
    volumes = cfdGetVolumesForElements;    
    
    % Another approach which takes the transient term into consideration
    if cfdIsTransient
        rho = cfdGetDataArray('rho');
        if strcmp(theEquationName, 'T')
            Cp = cfdGetDataArray('Cp');
            rho = rho .* Cp;
        end        
        deltaT = cfdGetDeltaT;
        
        theMaxResidualSquared = 0.;
        theMaxScaledResidual = 0;
        for iElement=1:theNumberOfElements
            volume = volumes(iElement);
            local_ac = ac(iElement);
            if cfdIsTransient
                at = volume*rho(iElement)/deltaT;
                local_ac = local_ac - at;
                if(local_ac < 1e-6*at)
                    local_ac = at;
                end
            end
            local_residual = bc(iElement);
            local_residual = local_residual/(local_ac*scale);
            theMaxScaledResidual = max(theMaxScaledResidual,abs(local_residual));
            theMaxResidualSquared = theMaxResidualSquared + local_residual*local_residual;
        end
        %
        maxScaledResidual = theMaxScaledResidual;
        rmsScaledResidual = sqrt(theMaxResidualSquared/theNumberOfElements);        
    else
        theMaxResidualSquared = 0.;
        theMaxScaledResidual = 0;
        for iElement=1:theNumberOfElements
            local_residual = bc(iElement);
            local_residual = local_residual/(ac(iElement)*scale);
            %
            theMaxScaledResidual = max(theMaxScaledResidual,abs(local_residual));
            theMaxResidualSquared = theMaxResidualSquared + local_residual*local_residual;
        end
        %
        maxScaledResidual = theMaxScaledResidual;
        rmsScaledResidual = sqrt(theMaxResidualSquared/theNumberOfElements);        
    end    
end

cfdSetEquationResiduals(theEquationName, rmsScaledResidual, maxScaledResidual, iComponent);


