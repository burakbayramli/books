function [ x, dx ] = getMesh( r, l, mesh_size )
dx = abs(r-l)/mesh_size;
x = linspace (l + dx/2, r - dx/2, mesh_size); % space discretization
x = [x(1)-dx x x(end)+dx]; % ghost cells for bc
% ghost cells
end
