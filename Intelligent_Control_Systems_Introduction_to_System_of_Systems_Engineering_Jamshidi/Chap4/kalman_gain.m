
function K_k = kalman_gain(P_k_apriori,H_k,R_k); 

K_k = P_k_apriori*H_k'*inv(H_k*P_k_apriori*H_k' + R_k);

