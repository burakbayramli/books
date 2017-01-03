%this m-file performs a mixture analysis 
%using the class test score data. 

%let p1 denote the probability of success in the first component
%p2 denote the probability of success in the second
%theta denote the probability of the first componet
%tau denote the component indicator variable.

clear;
clc;
rand('seed',sum(100*clock));
randn('seed',sum(100*clock));

%LOAD IN THE DATA
T=[ 22  20  21  20  23  21  20  17  21  20 ... 
21  19  21  21  20  20  20  17  19  19 ... 
19  21  19  22  24  19  6  17  21  24 ... 
20  20  21  22  19  20  19  20  19  18 ... 
23  21  19  19  21  19  21  17  19  20 ... 
23  21  21  8  19  21  21  20  18  16 ... 
18  21  17  22  21  22  19  19  19  22 ... 
23  23  18  7  21  21  19  18  19  5 ... 
21  20  17  20  20  19  20  19  24  18 ... 
20  5  5  19  18  19  20  20  22  22 ... 
21  19  22  23  21  16  21  20  21  20 ... 
23  4  18  16  23  20  21  22  22  21 ... 
22  17  21  6  23  20  18  19  20  23 ... 
22  3  21  23  19  21  21  19  23  18 ... 
19  21  21  19  17  20  21  24  14  17 ... 
20  17  19  21  20  22  21  18  22  4 ... 
19  16  18  17  24  21  22  3  16  21 ... 
19  22  22  20  20  20  20  16  18  19 ... 
20  21  22  18  21  19  16  22  18  18 ... 
23  18  20  24  19  16  18  5  23  23 ]';
iter=1000;
nobs=200;
nquest=25;




%--------
%set prior values
%--------
t1 = 2;
t2 = 2;    %parms for pi prior

a1 = 2;
a2 = 2;

b1 = 2;
b2 = 2;
%--------
%set matrix sizes
%--------------
p1 = zeros(iter,1);
p2 = p1;
pie = p1;
%----------------------
%set initial conditions
%----------------------
p1(1) = .3;
p2(1) = .7;
pie(1) = .4;
for i = 2:iter;i
    
    %-----------------
    %conditional for tau's - component indicators
    %-----------------
    
    pdf_1 = binopdf(T,nquest,p1(i-1));
    pdf_2 = binopdf(T,nquest,p2(i-1));
    probs = (pie(i-1)*pdf_1)./(pie(i-1)*pdf_1 + (1-pie(i-1))*pdf_2);
    uniforms = rand(nobs,1);
    tau = .5*sign(probs - uniforms) + .5;
    
    num_tau = sum(tau);
    %-------------------
    %conditional for pie
    %-------------------
    
    pie(i,1) = betarnd(num_tau + t1, nobs-num_tau + t2);
    %-------------------
    %conditional for theta_1, theta_2
    %-------------------
    p1(i) = betarnd(sum(tau.*T) + a1,  sum(tau.*(nquest-T))+ b1);
    p2(i) = betarnd(sum((1-tau).*T) + a2, sum((1-tau).*(nquest-T) )+ b2);
end;
burn = 200;
%Plot mixture at mean values
disp('Mean Probability of success in first_component');
mu_t1 = mean(p1(burn:iter))
disp('Mean probability of success in second component');
mu_t2 = mean(p2(burn:iter))
disp('Probability associated with first_component');
mu_pie = mean(pie(burn:iter))
grids = linspace(1,25,25)';
dens_mix = mu_pie*binopdf(grids,nquest,mu_t1) + (1-mu_pie)*binopdf(grids,nquest,mu_t2);
plot(grids,dens_mix)
hold on;
tempp = tabulate(T);
probs1 = tempp(:,2)/sum(tempp(:,2));
plot(grids(1:24),probs1(1:24),':');
xlabel('Number of Questions Correct');
ylabel('Density');
hold off;

%do the predictive exercise
m = length(pie);
yf = [8 12 22]';

for j = 1:length(yf);
    num = mean( pie.*(p1.^yf(j)).*( (1-p1).^(nquest-yf(j))) );
    denom = mean ( pie.*(p1.^yf(j)).*( (1-p1).^(nquest-yf(j))) + ...
        ( (1-pie).*(p2.^yf(j)).*( (1-p2).^(nquest-yf(j))) ) );
    probs_rand(j,1) = num/denom;
end;
probs_rand