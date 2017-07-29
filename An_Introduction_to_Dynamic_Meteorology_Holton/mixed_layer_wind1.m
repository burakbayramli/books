% MATLAB file:   mixed_layer_wind1.m    (rev: 5/20/2002)
% Demo script to iteratively solve eqs. (5.20) and (5.21)
% for u and v using geostrophic wind as first guess.
% Geostrophic wind, ug, varies from 1 to 20 m/s
% Note that solution does not converge for ug greater than 19 m/s
clear all
close all
V = zeros(20,2);                    % Vector for components u, v
ug = zeros(1,20);
Vb= V;                              % Magnitude of mixed layer wind

for j = 1:20  
    ug(j) = j;                      % Geostrophic wind in increments of 1 m/s
    vg = 0;
    kappa = 0.05;                   % coefficient for mixed layer s/m
    %initialize u, v to geostrophic values
    u = ug(j);
    v = vg;
    V(j,:) = [u v] ;                % vector wind
    Vb(j) = abs(+V(j,1)+i*V(j,2));  % initialize wind magnitude to geostrophic
    
    % Iterate to find boundary layer solution
    
    for n = 1:12
        v = vg +kappa*Vb(j)*u;    
        u = ug(j) - kappa*Vb(j)*v;
        
        Vb(j) = abs(+u+i*v) ;        % new estimate for Vb
        V(j,:) = [u v];              % new estimate for V
    end                             
end

[ug',V]
plot(ug,V(:,1), '--',ug,V(:,2),':',ug, Vb, 'k',ug,ug,'*')
xlabel('geostrophic wind (m/s)');
ylabel('speed (m/s)');
title('ug (stars); V (solid); u (dashed); v (dotted)');

