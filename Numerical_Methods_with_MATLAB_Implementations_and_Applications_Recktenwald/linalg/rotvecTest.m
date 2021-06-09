% rotvectTest  Use the rotvec function to rotate some vectors

ui = [1 0 0]';
disp('flip [1 0 0] through 90 about z axis');    u = rotvec(ui,0,0,90)

disp('flip [1 0 0] through 180 about y axis');   u = rotvec(ui,0,180,0)

ui = [1 1 1]';
disp('flip [1 1 1] through 45 about all axes');  u = rotvec(ui,45,45,45)

ui = [-2 3 1]';
disp('flip [-2 3 1] through 30 about z axis');   u = rotvec(ui,0,0,30)

% --- demonstrate that length is unaffected by rotation
uf = rotvec(ui,25,-145,202);
norm(ui)
norm(uf)

