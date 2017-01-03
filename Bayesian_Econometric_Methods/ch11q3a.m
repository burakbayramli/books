%Matlab code for part a) Question 3 of Chapter 11
%Generate an artificial data set

%set n, beta(1), beta(2) and h to appropriate values
n=100;
beta1 = 0;
beta2=1;
h=1;
sigma=sqrt(1/h);

%now generate the data
%set first element of x to be an intercept
x(:,1)=ones(n,1);
for i=1:n
    x(i,2)=rand;
    epsilon= sigma*randn;
    y(i,1) = beta1 + beta2*x(i,2) + epsilon;
end


%Now save the data in a file for future use
data = [y x];
save ch2q2.out data -ASCII;