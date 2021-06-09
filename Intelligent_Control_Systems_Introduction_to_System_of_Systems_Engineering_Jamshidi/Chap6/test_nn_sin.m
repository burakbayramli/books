clear all;

load inertia_weight;
load coriolis_weight;
load centri_weight;
load gravity_weight;
load coriolis_ctheta;
load centri_ctheta;
load inertia_ctheta;
load gravity_ctheta;

T = 0.002;
Time_span = 100;
eta_c = 0.000001;
%We have one neural network to map proprioceptive information to one
%element in the inertia, velocity, and gravity matrices.
%Then we define the number of neurons in each network
num_neurons_inertia = 5;
num_neurons_velocity = 5;
num_neurons_gravity = 5;

sigma_theta = 1;
sigma_theta_dot = 4;

error_collect = [];
    for m = 1:2
        gravity_modification(m) = 0;
        for n = 1:2
            inertia_modification(m,n) = 0;
            coriolis_modification(m,n) = 0;
            centri_modification(m,n) = 0;
        end
    end
for numb = 1:3
        data_collect = [];
        load torque_data;
        
        tau_data = torque_data(numb).d;
        
        %     Fp1 = fopen(file_name,'r');
        %     tau_data = fscanf(Fp1,'%12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f %12.6f\n');
        
        time = tau_data(:,1);
        torque = tau_data(:,[2 3]);
        thetad = tau_data(:,[4 5]);
        theta_dot_d = tau_data(:,[6 7]);
        theta_ddot_d = tau_data(:,[8 9]);
        
        
        inertia_element = [];
        
        for i =1:length(time)
            
            for m = 1:2
                for n = 1: 2 - (m-1)
                    iner_weight = inertia_weight(m,n).w;
                    inertia_activ_collect = [];
                    activation = hidden_layer_inertia_sin(thetad(i,:));
                    inertia_activ_collect = activation;
                    inertia_activity(m,n).act = inertia_activ_collect;
                    inertia_element(m,n) = iner_weight'*inertia_activ_collect;
                    inertia_element(n,m) = inertia_element(m,n);
                end
            end
            
            inertia_torque = inertia_element*theta_ddot_d(i,:)';
            %velocity network
            
            for m = 1:2
                for n = 1:2
                    cor_weight = coriolis_weight(m,n).w;
                    coriolis_activ_collect = [];
                    activation = hidden_layer_coriolis_sin(thetad(i,:));
                    coriolis_activ_collect = activation;
                    coriolis_activity(m,n).act = coriolis_activ_collect;
                    coriolis_element(m,n) = cor_weight'*coriolis_activ_collect;
                end
            end
            coriolis_torque = coriolis_element*[theta_dot_d(i,1)*theta_dot_d(i,2);theta_dot_d(i,1)*theta_dot_d(i,2)];
            
            for m = 1:2
                for n = 1:2
                    cen_weight = centri_weight(m,n).w;
                    centri_activ_collect = [];
                    activation = hidden_layer_centri_sin(thetad(i,:));
                    centri_activ_collect = activation;
                    centri_activity(m,n).act = centri_activ_collect;
                    centri_element(m,n) = cen_weight'*centri_activ_collect;
                end
            end
            centri_torque = centri_element*[theta_dot_d(i,1)^2;theta_dot_d(i,2)^2];
            
            sigma_theta_dot = 0.5;
            %Gravity network
            for m = 1:2
                
                grav_weight = gravity_weight(m).w;
                gravity_activ_collect = [];
                
                activation = hidden_layer_gravity_sin(thetad(i,:));
                gravity_activ_collect = activation;
                
                gravity_activity(m).act = gravity_activ_collect;
                gravity_element(m) = grav_weight'*gravity_activ_collect;
            end
            gravity_torque = gravity_element';
            joint_torque = inertia_torque + coriolis_torque + centri_torque + gravity_torque;
            torque_error = torque(i,:)' - joint_torque;
             data_collect = [data_collect;[i*T torque_error'*torque_error torque(i,:) joint_torque']];
        error_collect = [error_collect;torque_error'*torque_error];
            
           
            
        end
        
        
    size(data_collect)
    
    time = data_collect(:,1);
    torque_error = data_collect(:,2);
    torqued1 = data_collect(:,3);
    torqued2 = data_collect(:,4);
    torque1 = data_collect(:,5);
    torque2 = data_collect(:,6);
    figure;
    subplot(3,1,1)
    plot(time,torqued1,time,torque1);
    legend('Desired','Real');
    ylabel('Joint 1 torques [Nm]');
    subplot(3,1,2)
    plot(time,torqued2,time,torque2);
    legend('Desired','Real');
    ylabel('Joint 2 torques [Nm]');
    subplot(3,1,3)
    plot(time,torque_error);
    xlabel('Time [sec]');
    ylabel('Quad. torque error [Nm]^2');
end