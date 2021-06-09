clear all;
T = 0.002;

Time_span = 100;

for file_number = 1:3
    thetad = [];
    theta_dot_d = [];
    theta_ddot_d = [];
    traj_para = randn;
    for i = 1:Time_span
        theta_d1 = -(0.5+traj_para)*pi/8 + pi/4*sin(2*pi*T*i);
        theta_d2 = -(0.5+traj_para)*pi/6 + pi/3*sin(2*pi*T*i + pi/6*(0.5+traj_para));
        thetad = [thetad;[theta_d1 theta_d2]];
    end

    theta_dot_d = diff(thetad) ./ T;
    theta_ddot_d = diff(theta_dot_d) ./ T;
    
    theta_dot_d = [theta_dot_d;repmat(theta_dot_d(end,:),(Time_span-length(theta_dot_d)),1)];
    theta_ddot_d = [theta_ddot_d;repmat(theta_ddot_d(end,:),(Time_span-length(theta_ddot_d)),1)];
    
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
    
    for k = 1:Time_span
        M = inertia(iner_para,thetad(k,:));
        V = coriolis(coriolis_para,thetad(k,:),theta_dot_d(k,:));
        G = gravity(gravity_para,thetad(k,:));
        tau = M*theta_ddot_d(k,:)' + V + G;
        
%         tau = torque(theta_ddot_d(k,:), M, V, G);
        
        tau_data = [tau_data;[T*k tau' thetad(k,:) theta_dot_d(k,:) theta_ddot_d(k,:)]];
    end
    torque_data(file_number).d = tau_data;
    
    
    % fprintf(Fp1,'%12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f',tau_data);
    % fclose(Fp1);
end
save torque_data;