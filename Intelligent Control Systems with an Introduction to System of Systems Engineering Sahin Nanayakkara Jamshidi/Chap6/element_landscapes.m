clear all;
T = 0.002;

Time_span = 100;

iner_para.p1 = 0.0363;
iner_para.p2 = 0.006;
iner_para.p3 = 0.00135;
iner_para.p4 = 0.003;
iner_para.p5 = 0.0023;

coriolis_para.p1 = 0.003;
coriolis_para.p2 = 0.0981;

gravity_para.p1 = 0.0981;
gravity_para.p2 = 0.8829;
gravity_para.p3 = 0.0981;

M11 = [];
M12 = [];
M21 = [];
M22 = [];

CO1 = [];
CO2 = [];

G1 = [];
G2 = [];

CE11 = [];
CE12 = [];
CE21 = [];
CE22 = [];

for i = 1:100
    for j = 1:100
        thetad = [(-0.5 + 0.02*i) (-0.5 +0.02*j)];
        M = inertia(iner_para,thetad);
        M11(i,j) = M(1,1);
        M12(i,j) = M(1,2);
        M21(i,j) = M(2,1);
        M22(i,j) = M(2,2);
        
        CO1(i,j) = -coriolis_para.p1*sin(thetad(2));
        CO2(i,j) = 0;
        
        CE11(i,j) = 0;
        CE12(i,j) = -coriolis_para.p1*sin(thetad(2));
        CE21(i,j) = coriolis_para.p2*sin(thetad(2));
        CE22(i,j) = 0;
        
        G1(i,j) = gravity_para.p1*sin(sum(thetad)) + gravity_para.p2*sin(thetad(1));
        G2(i,j) = gravity_para.p3*sin(sum(thetad));
    end
end

theta1 = [-0.48: 0.02:1.5];
theta2 = [-0.48: 0.02:1.5];
figure;
subplot(2,2,1)
mesh(theta1,theta2,M11);
title('M11');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,2)
mesh(theta1,theta2,M12);
title('M12');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,3)
mesh(theta1,theta2,M21);
title('M21');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,4)
mesh(theta1,theta2,M22);
title('M22');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

figure;
subplot(2,2,1)
mesh(theta1,theta2,CE11);
title('CE11');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,2)
mesh(theta1,theta2,CE12);
title('CE12');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,3)
mesh(theta1,theta2,CE21);
title('CE21');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,4)
mesh(theta1,theta2,CE22);
title('CE22');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

figure;
subplot(2,2,1)
mesh(theta1,theta2,CO1);
title('CO1');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,2)
mesh(theta1,theta2,CO2);
title('CO2');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,3)
mesh(theta1,theta2,G1);
title('G1');
xlabel('Joint angle 2');
ylabel('Joint angle 1');

subplot(2,2,4)
mesh(theta1,theta2,G2);
title('G2');
xlabel('Joint angle 2');
ylabel('Joint angle 1');
end
