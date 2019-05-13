
function P_k = error_covariance(K_k,H_k,P_k_apriori)

P_k = (eye(length(H_k)) - K_k*H_k)*P_k_apriori;