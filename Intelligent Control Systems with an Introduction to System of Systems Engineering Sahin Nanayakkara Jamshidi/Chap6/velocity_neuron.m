function activation = velocity_neuron(theta,theta_dot,sigma_theta,sigma_theta_dot,centre_theta,centre_theta_dot)

activation = [1];
for i = 1:length(centre_theta)
x1 = theta' - centre_theta(i,:)';
x2 = theta_dot' - centre_theta_dot(i,:)';

activation1 = (1/(sqrt(2*pi)*sigma_theta^3))*(1 - x1'*x1/sigma_theta^2)*exp(-(x1'*x1/(2*sigma_theta^2)));
activation2 = (1/(sqrt(2*pi)*sigma_theta_dot^3))*(1 - x2'*x2/sigma_theta_dot^2)*exp(-(x2'*x2/(2*sigma_theta_dot^2)));

activation = [activation;activation1*activation2];
end




