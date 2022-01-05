clear;
format compact
psi = @(x) x.^3 - x;   % potential function (RHS)

kernel = @(x,y) abs(x-y); 

n=10;
h=2/n;

x=linspace(-1,1,n+1);
x_c = x(1:end-1)+h/2;

%% 1st kind integral equation
% Construct matrix K
K=zeros(n,n);
for i=1:n
    for j=1:n
        K(i,j) = h/2*kernel(x_c(i),x_c(j)-h/4)+ ...
            h/2*kernel(x_c(i),x_c(j)+h/4);
    end
end

b = psi(x_c)'; % RHS

sigma = K\b; % Solve system
cond_1st_kind = cond(K);  % condition number

% Evaluate solution on finer grid
nEval = 1000;
xEval=linspace(-1,1,nEval);

sigmaEval = zeros(nEval,1);
count=2;
for i=1:nEval
    if xEval(i) <= x(count)
        %count stays same
    else
        count=count+1;
    end
    sigmaEval(i) = sigma(count-1);
end

plot(xEval,sigmaEval)


%% 2nd kind integral equation
A = eye(n) + K;  % identity plus K operator

sigma_2 = A\b; % solve system


% Evaluate solution on finer grid
sigmaEval_2 = zeros(nEval,1);
count=2;
for i=1:nEval
    if xEval(i) <= x(count)
        %count stays same
    else
        count=count+1;
    end
    sigmaEval_2(i) = sigma_2(count-1);
end

figure
plot(xEval,sigmaEval_2)

cond_2nd_kind = cond(A);

%% Convergence of 2nd kind IE with n
% Now we might ask: how does the accuracy improve as n is increased?
% Let's do a small convergence study to find out.
n_conv = [10 20 40 80 160 320 640];

% Construct a matrix, each row of which contains the solution for a
% different n evaluated at 1000 equally spaced points
nEval = 1000;
xEval=linspace(-1,1,nEval);
sigmaEval = zeros(nEval,length(n_conv));

for i_conv = 1:length(n_conv)
    n = n_conv(i_conv);
    h=2/n;
    x=linspace(-1,1,n+1);
    x_c = x(1:end-1)+h/2;
    K=zeros(n,n);
    for i=1:n
        for j=1:n
            K(i,j) = h/2*kernel(x_c(i),x_c(j)-h/4)+ ...
                h/2*kernel(x_c(i),x_c(j)+h/4);
        end
    end
    b = psi(x_c)'; % RHS
    A = eye(n) + K;  % identity plus K operator
    sigma = A\b;
    % Evaluate solution on finer grid

    count=2;
    for i=1:nEval
        if xEval(i) <= x(count)
            %count stays same
        else
            count=count+1;
        end
        sigmaEval(i,i_conv) = sigma(count-1);
    end
end

% Plot solution at each level of discretization - is it converging?
figure
plot(xEval,sigmaEval)

% Now let's evaluate the error for each n, using the solution obtained via 
% the finest discretization as the reference solution.
for i_conv=1:length(n_conv)-1
    rel_err(i_conv) = norm(sigmaEval(:,i_conv)-sigmaEval(:,end))/norm(sigmaEval(:,end));
end

% Plot this on a loglog plot
figure
loglog(n_conv(1:end-1),rel_err)
% Plot alongside 1/n curve to see that scheme converges as 1/n
hold on
loglog(n_conv(1:end-1),n_conv(1:end-1).^(-1))


%% Nystrom method - let's use Gauss-Legendre nodes and weights
kernel = @(x,y) (x-y).^2;  % consider a smoother kernel for Nystrom
nG = 40;
[xG,wG] = gauss(nG);

K_N = zeros(nG,nG);
for i=1:nG
    for j=1:nG
        K_N(i,j) = wG(j) * kernel(xG(i),xG(j));
    end
end

A_N = eye(nG)+K_N;

b_N = psi(xG);

sigma_N=A_N\b_N;

figure
plot(xG,sigma_N)

% The solution should be as accuate as the quadrature scheme, e.g.,
% exponentially accurate in n for Gauss-Legendre. HOWEVER, the problem is
% that the solution is only given at quadrature nodes. This makes it hard
% to compare solutions at different values of n.







        
