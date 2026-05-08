function tc = find_cut_cells(segment,dh,cut_start_y,cut_end_x)
% returns a vector of handles to each touched_cell.
% gets the indices of the cells touched by segment.
% We assume the segment starts on bottom and ends on left wall.

ray = segment(2,:) - segment(1,:);
nodes = round( 1 / dh );
tc = [];
% lasts = tc(:,1);
% lasts = zeros(nodes*3,1);
[i_start, ~] = cell_point(segment(1,:), dh);
% we assume we have a valid start point.
counter = 1;
finished = 0;
% last_j = 1;
normal = [ray(2); -ray(1)] / norm(ray,2);
for j = 1:nodes
    % lets go row by row.
    checking = 1;
    i = i_start;
    while checking
        % check backwards until we have reached a non-touched cell in this row.
        if i_start < 1
            break
        end
        xmin = -dh/2 + (i-1)*dh;
        ymin = -dh/2 + (j-1)*dh;
        if intersect_segment_cell(segment(1,:), ray, [xmin, ymin], dh)
            if i < 1
                finished = 1;
                break
            end
            
            % Cell fluid area subroutine
            points = cell_intersections(segment,[xmin,ymin],dh);
            points(11,3) = 0;
            % assuming the segment touches this cell, the bottom corner must be out of
            %   the fluid domain, and the top corner must be in the fluid domain.
            % Check top left corner
            if cross2d([xmin,ymin+dh]-segment(1,:),ray) > 0
                points(5,:) = [xmin, ymin+dh, 1];
            end
            % Check bottom right corner
            if cross2d([xmin+dh,ymin]-segment(1,:),ray) > 0
                points(6,:) = [xmin+dh, ymin, 1];
            end
            % East wall, did not intersect top
            if xmin == -dh/2 && points(3,3) == 0
                points(7,:) = [0, ymin+dh, 1];
                points(8,:) = [0, cut_start_y, 1];
            end
            % Bottom wall, did not intersect east
            if ymin == -dh/2 && points(2,3) == 0
                points(9,:) = [xmin+dh, 0, 1];
                points(10,:) = [cut_end_x, 0, 1];
            end
            % top right corner.
            points(11,:) = [xmin+dh,ymin+dh,1];
            % disable intersections outside of fluid domain.
            points(points(:,1) < 0,3) = 0;
            points(points(:,2) < 0,3) = 0;
            points = points(points(:,3) == 1, 1:2);
            points = reorder_polygon_points(points);
            cell_fluid_area = polyarea( points(:,1), points(:,2) );
            % end cell area subroutine.
            
%             tc(counter) = touched_cell(i, j, cell_fluid_area);
            tc = [tc, touched_cell(i, j, cell_fluid_area, normal)];
            counter = counter + 1;
        elseif i == i_start
            i_start = i_start - 1;
        else
            checking = false;
        end
        i = i-1;
    end
%     if tc(counter-1,1)-1 > 0
%         lasts(j) = tc(counter-1,1)-1;
%         last_j = j;
%     end
    if finished
        break
    end
end
% tc = tc(1:counter-1);
% lasts = lasts(1:last_j);



