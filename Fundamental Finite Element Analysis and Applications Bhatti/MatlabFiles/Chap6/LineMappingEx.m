% Mapping for a cubic line defined by the following four points 
pts = [0,1; 3,3; 4,5; 5,6];
xn=pts(:,1); yn= pts(:,2);
map=[];
for s=-1:1/15:1
    % n = Cubic Lagrange interpolation functions 
    n = [(-9*(-1 + s)*(-1/3 + s)*(1/3 + s))/16, ...
            (27*(-1 + s)*(-1/3 + s)*(1 + s))/16, ...
            (-27*(-1 + s)*(1/3 + s)*(1 + s))/16, ...
            (9*(-1/3 + s)*(1/3 + s)*(1 + s))/16];
    x = n*xn; y=n*yn;
    map=[map; [x,y]];
end
fprintf('Coordinates of point on the mapped line');
map'
plot(map(:,1), map(:,2))