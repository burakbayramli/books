function tau = torque(theta_ddot,M,V,G)

tau = M*theta_ddot' + V + G;