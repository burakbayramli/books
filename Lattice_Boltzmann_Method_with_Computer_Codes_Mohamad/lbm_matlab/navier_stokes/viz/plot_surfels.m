function plot_surfels(ss,nodes,dh)
% plots the pgrams associated with all surfels.

figure;
hold on;
axis equal tight;
plot_lattice_lines(nodes);
stride = 3;
for k = 1:stride:length(ss)
    s = ss(k);
    p0 = s.startp;
    v1 = s.segment;
    for p = s.pgrams
        v2 = p.extrusion;
        plot_pgram(p0,v1,v2);
        [bmin, bmax, ~, ~] = pgram_bounds(p0, v1, v2,dh);
        plot_bounding_box(bmin,bmax);
    end
end
% plot([cut_end_x,0],[0,cut_start_y]);

% The old version
% % Plot surfels!!! Surfel and lattice check.
% figure;
% hold on;
% plot_lattice_lines(nodes);
% [considered, ~] = size(c_wall);
% for lv = 1:considered
%     for k = 1:3:surfels
%         plot_surfel(p0(k,:), v1, v2(lv,:));
%         [bmin, bmax, imin, imax] = pgram_bounds(p0(k,:), v1, v2(lv,:),dh);
%         plot_bounding_box(bmin,bmax);
%     end
% end
% plot([cut_end_x,0],[0,cut_start_y]);