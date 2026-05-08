function [vectors, components] = relevant_lattice_speeds(normal)
% normal: normal vector from wall segment pointing INTO the fluid domain.

unit_normal = normal / norm(normal,2);
c = zeros(9,2);
c(1,:) = [0, 0];
c(2,:) = [1, 0];
c(3,:) = [0, 1];
c(4,:) = [-1, 0];
c(5,:) = [0, -1];
c(6,:) = [1, 1];
c(7,:) = [-1, 1];
c(8,:) = [-1, -1];
c(9,:) = [1, -1];
valid = zeros(9,1);
for k = 1:9
    valid(k) = dot(unit_normal,c(k,:)) < 0;
end
components = find(valid==1); % the components relevant to wall.
% lattice velocity components to use on wall 
% (pointing INTO wall FROM fluid domain.).
vectors = c( valid == 1 , : );