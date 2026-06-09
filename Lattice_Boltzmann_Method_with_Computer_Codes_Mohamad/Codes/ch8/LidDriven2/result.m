function result(nx, ny, x, y, u, v, uo, rho)

mid_x = (nx - 1) / 2;   % index 50 (1-based: 51)
mid_y = (ny - 1) / 2;

% Centreline profiles
um = u(mid_x+1, :) / uo;   % u along vertical centreline vs y
vm = v(:, mid_y+1) / uo;   % v along horizontal centreline vs x

% --- U-velocity centreline profile ---
figure;
plot(um, y, 'b-', 'LineWidth', 1.5);
hold on;
line([0 0], [0 1], 'Color', 'k', 'LineStyle', '--', 'LineWidth', 0.5);
xlabel('U / U_0'); ylabel('Y');
title(sprintf('Centreline U-velocity  (x = %.2f)', x(mid_x+1)));
grid on;

% --- V-velocity centreline profile ---
figure;
plot(x, vm, 'r-', 'LineWidth', 1.5);
hold on;
line([0 1], [0 0], 'Color', 'k', 'LineStyle', '--', 'LineWidth', 0.5);
xlabel('X'); ylabel('V / U_0');
title(sprintf('Centreline V-velocity  (y = %.2f)', y(mid_y+1)));
grid on;

% --- Quiver plot ---
s = 4;   % subsample every 4th node
xq = x(1:s:end);
yq = y(1:s:end);
Uq = u(1:s:end, 1:s:end)' / uo;   % transpose: rows=y, cols=x
Vq = v(1:s:end, 1:s:end)' / uo;

figure;
[Xq, Yq] = meshgrid(xq, yq);
quiver(Xq, Yq, Uq, Vq, 1.5);
axis([0 1 0 1]);
axis equal;
xlabel('X'); ylabel('Y');
title('Lid-Driven Cavity – Velocity Quiver');

% --- Stream function contours ---
str = zeros(nx, ny);
for j = 2:ny
    str(:, j) = str(:, j-1) + 0.25 * (rho(:,j) + rho(:,j-1)) .* (u(:,j) + u(:,j-1));
end

figure;
contour(x, y, str', 20);
xlabel('X'); ylabel('Y');
title('Stream Function Contours');

% --- Streamline plot (replaces out0.jpg) ---
figure;
[X, Y] = meshgrid(x, y);
s = 4;
quiver(X(1:s:end, 1:s:end), Y(1:s:end, 1:s:end), ...
       u(1:s:end, 1:s:end)', v(1:s:end, 1:s:end)', 1.5);
axis([0 1 0 1]);
axis equal;
xlabel('X'); ylabel('Y');
title(sprintf('Lid-Driven Cavity Flow  (Re = %.0f)', uo*(ny-1)/0.1));

end
