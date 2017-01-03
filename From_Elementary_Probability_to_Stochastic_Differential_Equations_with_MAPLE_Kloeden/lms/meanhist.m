%MEANHIST.M      Compute mean exit time via Monte Carlo and 
%                plot a histogram of the sample exit times.
%                Simulation is for a linear SDE with known solution paths.
%

clf
randn('state',100)            % set the state of randn
Dt = 1e-2;                    % stepsize
M = 5e4;                      % number of paths

mu = 0.1; sigma = 0.2;        % problem parameters
a = 0.5;                      %
b = 2;                        %
Xzero = 1;                    %

texit = zeros(M,1);
for s = 1:M
     s
     X = Xzero;
     t = 0;
     while X > a & X < b,
       dW = sqrt(Dt)*randn;                          % Brownian increments
       X = X*exp( (mu - 0.5*sigma^2)*Dt + sigma*dW); % Exact solution 
       t = t + Dt;
     end
     texit(s) = t - 0.5*Dt;
end

tmean = mean(texit)
tstd = std(texit)
cileft = tmean - 1.96*tstd/sqrt(M)
ciright = tmean + 1.96*tstd/sqrt(M)

%%% Compute exact value %%% 
%%% Use intermediate variables to break up the formula %%%
temp1 = 1/(0.5*sigma^2 - mu);
temp2 = log(Xzero/a);
power = 1 - 2*mu/(sigma^2);
temp3 = 1 - (Xzero/a)^power;
temp4 = 1 - (b/a)^power;
temp5 = log(b/a);
texact = temp1*( temp2 - (temp3/temp4)*temp5)

hist(texit,50)    % histogram
xlabel('First exit time','FontSize',16,'FontWeight','Bold') 
set(gca,'FontWeight','Bold','FontSize',12)

