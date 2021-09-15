% calc_yhat_kinetic_ex1.m
function y_hat = calc_yhat_kinetic_ex1(theta, X_pred);

N = size(X_pred,1);  % # of experiments

% extract predictors
conc_A = X_pred(:,1);  % [A] in each experiment
conc_B = X_pred(:,2);  % [B] in each experiment

% extract parameters
k_1 = theta(1);  % rate constant
nu_a = theta(2);  % exponent for [A]
nu_b = theta(3); % exponent for [B]


% make predictions
y_hat = zeros(N,1);
for k=1:N
    y_hat(k) = k_1*conc_A(k)^nu_a*conc_B(k)^nu_b;
end
% NOTE, WE COULD SIMPLY USE
% yhat = k1.*(conc_A.^nu_A).*(conc_B.^nu_B);
% TO AVOID SLOW FOR LOOP

return;
