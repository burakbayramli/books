% Function to calculate induced velocities and influence coefficients
%
% Anthony Ricciardi
% December 2014
%
% Inputs
% FEM = [struct] model data
%
% Outputs
% w:      Matrix of influnce coefficients.  Flow through each panel induced
%         proportionally by each vortex [number of panels x number of panels]
%
% Vind14: Velocity at the 1/4 chord of each panel induced proportionally by
%         each vortex [number of panels x number of panels x 3]
%
function [w, Vind14] = VoLAre_induced(FEM)

nb = size(FEM.boxes,2);
w = zeros(nb);
Vind14 = zeros(nb,3);

% symmetry condition
switch FEM.AEROZ.XZSYM
    case 'YES'
        symmetric = 1;
    case 'NO'
        symmetric = 0;
        vind14_sim = [0;0;0];
        vind34_sim = [0;0;0];
    otherwise
        error('AEROZ XZSYM option %s not supported.',FEM.AEROZ.XZSYM)
end

% calculate induced velocities and influences
for b1 = 1:nb
    b1i = FEM.boxes(2,b1); % macro element number
    b1j = FEM.boxes(3,b1); % box in macro element
    colloc14 = FEM.CAERO7(b1i).bound_cent(:,b1j);
    colloc34 = FEM.CAERO7(b1i).collocation(:,b1j);
    for b2 = 1:nb
        b2i = FEM.boxes(2,b2); % macro element number
        b2j = FEM.boxes(3,b2); % box in macro element
        
        vind14 = ...
            - trailingVortexSegment(FEM.CAERO7(b2i).bound_in(:,b2j),colloc14) ...
            + trailingVortexSegment(FEM.CAERO7(b2i).bound_out(:,b2j),colloc14)...
            + boundVortexSegment(FEM.CAERO7(b2i).bound_in(:,b2j),FEM.CAERO7(b2i).bound_out(:,b2j),colloc14);
        vind34 = ...
            - trailingVortexSegment(FEM.CAERO7(b2i).bound_in(:,b2j),colloc34) ...
            + trailingVortexSegment(FEM.CAERO7(b2i).bound_out(:,b2j),colloc34) ...
            + boundVortexSegment(FEM.CAERO7(b2i).bound_in(:,b2j),FEM.CAERO7(b2i).bound_out(:,b2j),colloc34);
        
        if symmetric
            vind14_sim = ...
                - trailingVortexSegment([1;-1;1].*FEM.CAERO7(b2i).bound_in(:,b2j),colloc14) ...
                + trailingVortexSegment([1;-1;1].*FEM.CAERO7(b2i).bound_out(:,b2j),colloc14)...
                + boundVortexSegment([1;-1;1].*FEM.CAERO7(b2i).bound_in(:,b2j),[1;-1;1].*FEM.CAERO7(b2i).bound_out(:,b2j),colloc14);
            vind34_sim = ...
                - trailingVortexSegment([1;-1;1].*FEM.CAERO7(b2i).bound_in(:,b2j),colloc34) ...
                + trailingVortexSegment([1;-1;1].*FEM.CAERO7(b2i).bound_out(:,b2j),colloc34)  ...
                + boundVortexSegment([1;-1;1].*FEM.CAERO7(b2i).bound_in(:,b2j),[1;-1;1].*FEM.CAERO7(b2i).bound_out(:,b2j),colloc34);
        end
        
        Vind14(b1,b2,1) = vind14(1) - vind14_sim(1);
        Vind14(b1,b2,2) = vind14(2) - vind14_sim(2);
        Vind14(b1,b2,3) = vind14(3) - vind14_sim(3);
        w(b1,b2) = FEM.CAERO7(b1i).n_vec(:,b1j)'*(vind34 - vind34_sim);
    end
end

end