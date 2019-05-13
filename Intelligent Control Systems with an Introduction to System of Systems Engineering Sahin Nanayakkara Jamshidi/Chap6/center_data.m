  for file_number = 1:1
        
        tau_data = load(['Torque_data' num2str(file_number) '.txt']);
        
        %     Fp1 = fopen(file_name,'r');
        %     tau_data = fscanf(Fp1,'%12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f\n');
        
        time = tau_data(:,1);
        torque = tau_data(:,[2 3]);
        thetad = tau_data(:,[4 5]);
        theta_dot_d = tau_data(:,[6 7]);
        theta_ddot_d = tau_data(:,[8 9]);
    plot(time,thetad(:,1),time,thetad(:,2));
end 