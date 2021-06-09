theta1 = [-2:0.01:2]';
theta2 = [-2:0.01:2]';
theta = [theta1 theta2];
centre_theta = [0.5 0.5];
sigma_theta = 0.5;
act = [];
for i = 1:length(theta1)
    for j = 1:length(theta2)
        x = [theta1(i) theta2(j)]' - centre_theta';
        act(i,j) = exp(-(x'*x/(2*sigma_theta^2)));
    end
end

mesh(theta1,theta2,act);
title('Centre: [0.5 0.5], width: 0.5');
xlabel('Joint-1 angle [rad]');
ylabel('Joint-2 angle [rad]');
zlabel('Activation');

