clear; close all; clc;

% % poly2cw test.
% n = 5+1;
% x = rand(n,1);
% y = rand(n,1);
% x(end) = x(1);
% y(end) = y(1);
% figure;
% hold on;
% plot(x(1:2),y(1:2));
% [xx, yy] = poly2cw(x,y);
% plot(xx(1:2),yy(1:2));
% figure;
% hold on;
% plot(x,y);
% plot(xx,yy);
% legend('random', 'ordered');

% % reorder_polygon_points test.
% n = 15+1;
% x = rand(n,1);
% y = rand(n,1);
% v = reorder_polygon_points([x,y]);
% plot(x,y,':');
% hold on;
% plot(v(:,1), v(:,2));
% legend('random', 'ordered');

% % vertex/cell in pgram test.
% p0 = [0;0];
% v1 = rand(2,1);
% v2 = rand(2,1);
% plot([p0(1), p0(1)+v1(1)], [p0(2), p0(2)+v1(2)]);
% hold on;
% plot([p0(1)+v1(1),  p0(1)+v1(1)+v2(1)], [p0(2)+v1(2), p0(2)+v1(2)+v2(2)]);
% plot([p0(1), p0(1)+v2(1)], [p0(2), p0(2)+v2(2)]);
% plot([p0(1)+v2(1), p0(1)+v2(1)+v1(1)], [p0(2)+v2(2), p0(2)+v2(2)+v1(2)]);
% cmin = rand(2,1);dh = 0.1;
% plot([cmin(1), cmin(1)+dh],[cmin(2), cmin(2)]);
% plot([cmin(1)+dh, cmin(1)],[cmin(2)+dh, cmin(2)+dh]);
% plot([cmin(1)+dh, cmin(1)+dh],[cmin(2)+dh, cmin(2)]);
% plot([cmin(1), cmin(1)],[cmin(2), cmin(2)+dh]);
% for k = 1:1000
%     point = rand(2,1);
%     if vertex_in_pgram(point, p0, v1, v2)
%         plot(point(1), point(2), 'go');
%     else
%         plot(point(1), point(2), 'rx');
%     end
%     if vertex_in_cell(point,cmin,dh)
%         plot(point(1), point(2), 'bo');
%     else
%         plot(point(1), point(2), 'mx');
%     end
% end

% Overlap area test.
p0 = [0;0];
v1 = rand(2,1);
v2 = rand(2,1);
p0 = [0.5;0];
v1 = [-0.5;0.5];
v2 = [0;1]* 0.0014;
dh = 0.0101;

% dh = 0.1;
% cmin = rand(2,1);
% cmin = (v1+v2)/2;
% cmin = (v1+v2)-dh/2;
cmin = [0;0.5];
area = overlap_pgram_cell(p0,v1,v2,cmin,dh);
disp([ 'Overlap area: ' num2str(area) ]);
plot([p0(1), p0(1)+v1(1)], [p0(2), p0(2)+v1(2)]);
hold on;
plot([p0(1)+v1(1),  p0(1)+v1(1)+v2(1)], [p0(2)+v1(2), p0(2)+v1(2)+v2(2)]);
plot([p0(1), p0(1)+v2(1)], [p0(2), p0(2)+v2(2)]);
plot([p0(1)+v2(1), p0(1)+v2(1)+v1(1)], [p0(2)+v2(2), p0(2)+v2(2)+v1(2)]);
plot([cmin(1), cmin(1)+dh],[cmin(2), cmin(2)]);
plot([cmin(1)+dh, cmin(1)],[cmin(2)+dh, cmin(2)+dh]);
plot([cmin(1)+dh, cmin(1)+dh],[cmin(2)+dh, cmin(2)]);
plot([cmin(1), cmin(1)],[cmin(2), cmin(2)+dh]);

