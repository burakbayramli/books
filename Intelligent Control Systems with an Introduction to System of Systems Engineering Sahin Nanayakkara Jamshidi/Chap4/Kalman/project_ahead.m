
function [x_next_apriori, P_next_apriori] = project_ahead(A,x_k_aposteriori,B,P_k,Q,w,u)

if i ==100
    w = 10*w;
end
x_next_apriori = A*x_k_aposteriori + B*u + w;
%P_k = exp(-0.001*P_k);
P_next_apriori = A*P_k*A' +  Q;
