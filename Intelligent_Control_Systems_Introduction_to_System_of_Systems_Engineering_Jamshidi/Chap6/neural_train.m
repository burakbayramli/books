clear all;
T = 0.002;
Time_span = 100;
eta_c = 0.001;
eta = 0.00001;
%We have one neural network to map proprioceptive information to one
%element in the inertia, velocity, and gravity matrices.
%Then we define the number of neurons in each network
num_neurons_inertia = 5;
num_neurons_velocity = 5;
num_neurons_gravity = 5;
%Initialize the centres and widths of the neurons
sigma_theta = 1.5;
sigma_theta_dot = 4;
for m = 1:2
    for n = 1: 2 - (m-1)
        inertia_ctheta(m,n).c = [0 0; 0.3 0.3; 0.6 0.6;0.8 0.8];
        inertia_weight(m,n).w = zeros(5,1);
    end
end


for m = 1:2
    for n = 1:2
        coriolis_ctheta(m,n).c = [0 0; 0.3 0.3; 0.6 0.6;0.8 0.8];
        coriolis_ctheta_dot(m,n).c = [0 0; 0.3 0.3; 0.6 0.6;0.8 0.8];
        coriolis_weight(m,n).w = zeros(5,1);
    end
end

for m = 1:2
    for n = 1:2
        centri_ctheta(m,n).c = [0 0; 0.3 0.3; 0.6 0.6;0.8 0.8];
        centri_ctheta_dot(m,n).c = [0 0; 0.3 0.3; 0.6 0.6;0.8 0.8];
        centri_weight(m,n).w = zeros(5,1);
    end
end

for m = 1:2
    gravity_ctheta(m).c = [0 0; 0.3 0.3; 0.6 0.6;0.8 0.8];
    gravity_weight(m).w = zeros(5,1);
end

mean_error3 = 0;
mean_error2 = 0;
mean_error1 = 0;
data = [];
for train = 1:10000
    
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
                    centre_theta = inertia_ctheta(m,n).c;
                    iner_weight = inertia_weight(m,n).w;
                    inertia_activ_collect = [];
                    activation = hidden_layer(thetad(i,:),sigma_theta,centre_theta);
                    inertia_activ_collect = activation;
                    [V,I] = max(inertia_activ_collect);
                    centre_theta(I,:) = centre_theta(I,:) + eta_c*V*(thetad(i,:) - centre_theta(I,:));
                    inertia_ctheta(m,n).c(I,:) = centre_theta(I,:);
                    inertia_activity(m,n).act = inertia_activ_collect;
                    inertia_element(m,n) = iner_weight'*inertia_activ_collect;
                    inertia_element(n,m) = inertia_element(m,n);
                end
            end
            
            inertia_torque = inertia_element*theta_ddot_d(i,:)';
            %velocity network
            
            for m = 1:2
                for n = 1:2
                    centre_theta = coriolis_ctheta(m,n).c;
                    centre_theta_dot = coriolis_ctheta_dot(m,n).c;
                    cor_weight = coriolis_weight(m,n).w;
                    coriolis_activ_collect = [];
                    activation = hidden_layer(thetad(i,:),sigma_theta,centre_theta);
                    coriolis_activ_collect = activation;
                    
                    [V,I] = max(coriolis_activ_collect);
                    centre_theta(I,:) = centre_theta(I,:) + eta_c*V*(thetad(i,:) - centre_theta(I,:));
                    coriolis_ctheta(m,n).c(I,:) = centre_theta(I,:);
                    
                    coriolis_activity(m,n).act = coriolis_activ_collect;
                    coriolis_element(m,n) = cor_weight'*coriolis_activ_collect;
                end
            end
            coriolis_torque = coriolis_element*[theta_dot_d(i,1)*theta_dot_d(i,2);theta_dot_d(i,1)*theta_dot_d(i,2)];
            
            for m = 1:2
                for n = 1:2
                    centre_theta = centri_ctheta(m,n).c;
                    centre_theta_dot = centri_ctheta_dot(m,n).c;
                    cen_weight = centri_weight(m,n).w;
                    centri_activ_collect = [];
                    activation = hidden_layer(thetad(i,:),sigma_theta,centre_theta);
                    centri_activ_collect = activation;
                    
                    [V,I] = max(centri_activ_collect);
                    centre_theta(I,:) = centre_theta(I,:) + eta_c*V*(thetad(i,:) - centre_theta(I,:));
                    centri_ctheta(m,n).c(I,:) = centre_theta(I,:);
                    
                    centri_activity(m,n).act = centri_activ_collect;
                    centri_element(m,n) = cen_weight'*centri_activ_collect;
                end
            end
            centri_torque = centri_element*[theta_dot_d(i,1)^2;theta_dot_d(i,2)^2];
            
            sigma_theta_dot = 0.5;
            %Gravity network
            for m = 1:2
                centre_theta = gravity_ctheta(m).c;
                grav_weight = gravity_weight(m).w;
                gravity_activ_collect = [];
                
                activation = hidden_layer(thetad(i,:),sigma_theta,centre_theta);
                gravity_activ_collect = activation;
                
                [V,I] = max(gravity_activ_collect);
                centre_theta(I,:) = centre_theta(I,:) + eta_c*V*(thetad(i,:) - centre_theta(I,:));
                gravity_ctheta(m).c(I,:) = centre_theta(I,:);
                
                gravity_activity(m).act = gravity_activ_collect;
                gravity_element(m) = grav_weight'*gravity_activ_collect;
            end
            gravity_torque = gravity_element';
            joint_torque = inertia_torque + coriolis_torque + centri_torque + gravity_torque;
            torque_error = torque(i,:)' - joint_torque;
            error_collect = [error_collect;torque_error'*torque_error];
            
            % Parameter update
            
            %Inertia
            for m = 1:2
                for n = 1: 2 - (m-1)
                    iner_weight = inertia_weight(m,n).w;
                    activity = inertia_activity(m,n).act;
                    inertia_modification(m,n,:) = mean(inertia_modification(m,n,:) + (torque_error(m)*activity*theta_ddot_d(i,m)));
                    inertia_modification(n,m,:) = inertia_modification(m,n,:);
                end
            end
            %Velocity
            for m = 1:2
                for n = 1:2
                    cor_weight = coriolis_weight(m,n).w;
                    activity = coriolis_activity(m,n).act;
                    coriolis_modification(m) = mean(coriolis_modification(m) + (torque_error(m)*activity*theta_dot_d(i,1)*theta_dot_d(i,2)));  
                end
            end
            for m = 1:2
                for n = 1:2
                    cen_weight = centri_weight(m,n).w;
                    activity = centri_activity(m,n).act;
                    centri_modification(m,n) = mean(centri_modification(m,n) + (torque_error(m)*activity*theta_dot_d(i,m)*theta_dot_d(i,m)));  
                end
            end
            %Gravity
            for m = 1:2
                grav_weight = gravity_weight(m).w;
                activity = gravity_activity(m).act;
                gravity_modification(m) = mean(gravity_modification(m) + (torque_error(m)*activity));   
            end
            
        end
        
    end
    
    for m = 1:2
        gravity_weight(m).w = gravity_weight(m).w + eta*gravity_modification(m);
        for n = 1:2
            coriolis_weight(m,n).w = coriolis_weight(m,n).w + eta*coriolis_modification(m,n);
            centri_weight(m,n).w = centri_weight(m,n).w + eta*centri_modification(m,n);
            inertia_weight(m,n).w = inertia_weight(m,n).w + eta*inertia_modification(m,n);
        end
    end
    mean_error3 = mean_error2;
    mean_error2 = mean_error1;
    mean_error1 = mean(error_collect);
    if mean_error1 > mean_error2
        eta_mod = 0.00000001;
        eta = 0.01*eta;
        for m = 1:2
            gravity_weight(m).w = gravity_weight(m).w + eta_mod*rand;
            for n = 1:2
                inertia_weight(m,n).w = inertia_weight(m,n).w + eta_mod*rand;
                coriolis_weight(m,n).w = coriolis_weight(m,n).w + eta_mod*rand;
                centri_weight(m,n).w = centri_weight(m,n).w + eta_mod*rand;
            end
        end
    end
    disp(['Trial ' num2str(train) ', error: ' num2str(mean(error_collect))]);
    data = [data; [train mean(error_collect)]];
end
plot(data(:,1),data(:,2));
xlabel('Number of trails');
ylabel('Mean quadratic error');
save data_wavelet;
save inertia_weight_wavelet;
save coriolis_weight_wavelet;
save centri_weight_wavelet;
save gravity_weight_wavelet;
save coriolis_ctheta_wavelet;
save centri_ctheta_wavelet;
save inertia_ctheta_wavelet;
save gravity_ctheta_wavelet;



