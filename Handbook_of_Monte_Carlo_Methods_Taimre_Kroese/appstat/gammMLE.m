%gammMLE.m
n = 100;
alpha = 3; lambda = 0.05;
x = gamrnd(alpha,1/lambda,1,n);
sumlogx = sum(log(x)); sumx = sum(x);
alp = mean(x)^2/var(x); lam = mean(x)/var(x); % initial guess
eta = [-lam;alp - 1]; S = Inf;
while  sum(abs(S) > 10^(-5)) > 0
    	S = [sumx + n*(eta(2) + 1)/eta(1); ...
	    	sumlogx + n*(log(-eta(1)) - psi(eta(2) + 1))];
    	I = n * [ (eta(2)+1)/eta(1)^2, -1/eta(1); ...
		-1/eta(1) , psi(1,eta(2)+1)];
    	eta = eta + I\S;
end
fprintf('lam_hat = %g , alpha_hat = %g \n',-eta(1),1+eta(2))

