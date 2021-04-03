function cfdPostScales
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function prints the scales to the screen
%--------------------------------------------------------------------------

global Region;

fprintf('**Scales:\n\n');

if cfdIsFieldAvailable('U')
    fprintf('U_max:     %f\n', Region.fluid.U.max);
    fprintf('U_min:     %f\n', Region.fluid.U.min);
    fprintf('U_scale:   %f\n\n', Region.fluid.U.scale);    
end

if cfdIsFieldAvailable('p')
    fprintf('p_max:     %f\n', Region.fluid.p.max);
    fprintf('p_min:     %f\n', Region.fluid.p.min);
    fprintf('p_scale:   %f\n\n', Region.fluid.p.scale);    
end

if cfdIsFieldAvailable('T')
    fprintf('T_max:     %f\n', Region.fluid.T.max);
    fprintf('T_min:     %f\n', Region.fluid.T.min);
    fprintf('T_scale:   %f\n\n', Region.fluid.T.scale);    
end

if cfdIsFieldAvailable('rho')
    fprintf('rho_max:   %f\n', Region.fluid.rho.max);
    fprintf('rho_min:   %f\n', Region.fluid.rho.min);
    fprintf('rho_scale: %f\n\n', Region.fluid.rho.scale);    
end

if cfdIsFieldAvailable('mu')
    fprintf('mu_max:    %f\n', Region.fluid.mu.max);
    fprintf('mu_min:    %f\n', Region.fluid.mu.min);
    fprintf('mu_scale:  %f\n\n', Region.fluid.mu.scale);    
end

if cfdIsFieldAvailable('Cp')
    fprintf('Cp_max:    %f\n', Region.fluid.Cp.max);
    fprintf('Cp_min:    %f\n', Region.fluid.Cp.min);
    fprintf('Cp_scale:  %f\n\n', Region.fluid.Cp.scale);    
end

if cfdIsFieldAvailable('k')
    fprintf('k_max:     %f\n', Region.fluid.k.max);
    fprintf('k_min:     %f\n', Region.fluid.k.min);
    fprintf('k_scale:   %f\n\n', Region.fluid.k.scale);    
end


fprintf('\n')



