T = 0.002;
format long;
thetad = [];
Time_span = 100;

for file_number = 1:20
for i = 1:Time_span
theta_d1 = -(0.5+rand)*pi/8 + (0.5+rand)*pi/4*sin(2*pi*T*(0.5+rand)*i);
theta_d2 = -(0.5+rand)*pi/6 + (0.5+rand)*pi/3*sin(2*pi*T*(0.5+rand)*i + pi/6*(0.5+rand));
thetad = [thetad;[theta_d1 theta_d2]];
end

theta_dot_d = diff(thetad) ./ T;
theta_ddot_d = diff(theta_dot_d) ./ T;

theta_dot_d = [theta_dot_d;repmat(theta_dot_d(end,:),(100-length(theta_dot_d)),1)];
theta_ddot_d = [theta_ddot_d;repmat(theta_ddot_d(end,:),(100-length(theta_ddot_d)),1)];

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

tau_data = [];
state_data = [];
state = [thetad(1,1),thetad(1,2),theta_dot_d(1,1),theta_dot_d(1,2)]';

file_name = ['Torque_data' num2str(file_number) '.txt'];
Fp1 = fopen(file_name,'w');

for i = 1:Time_span
    M_cont = inertia(iner_para,thetad(i,1),thetad(i,2));
    V_cont = coriolis(coriolis_para,thetad(i,1),thetad(i,2),theta_dot_d(i,1),theta_dot_d(i,2));
    G_cont = gravity(gravity_para,thetad(i,1),thetad(i,2));
    tau = torque(theta_ddot_d(i,1),theta_ddot_d(i,2),M_cont,V_cont,G_cont);
       
    tau_data = [tau_data;[T*i,tau' thetad(i,:) theta_dot_d(i,:) theta_ddot_d(i,:)]];
    
%     state = model(iner_para,coriolis_para,gravity_para,tau,state,T);
%     
%     state_data = [state_data;[T*i state' thetad(i,1),thetad(i,2)]];
    
end
fprintf(Fp1,'%12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f\n',tau_data);
fclose(Fp1);
end
