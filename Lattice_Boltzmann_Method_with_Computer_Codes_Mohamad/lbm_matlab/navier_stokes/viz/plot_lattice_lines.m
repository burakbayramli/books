function plot_lattice_lines(nodes)
% for the lid-driven cavity, where the cell centers are on the boundaries.

dh = 1/(nodes-1);
% plot_bounding_box([0,0],[1,1]);
current_x = -dh/2;
for k = 1:nodes
    plot([current_x, current_x],[-dh/2,1+dh/2],':');
    current_x = current_x + dh;
end
current_y = -dh/2;
for k = 1:nodes
    plot([-dh/2,1+dh/2],[current_y, current_y],':');
    current_y = current_y + dh;
end
    
    
