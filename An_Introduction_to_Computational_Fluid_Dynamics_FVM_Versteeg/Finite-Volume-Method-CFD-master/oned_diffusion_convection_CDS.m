%Code for 1d steady state diffusion and convection using FVM (CDS Scheme)
rho = 1; %density units must be kg/m^3
u = 0.1; %velocity units must be m/s
K = 0.1; %diffusion coeff units must be kg/m.s
L = 1; %length of 1d problem
n = 5; %node points
dx = L/n; %distance between points
phi_a = 1; %transport property at point a
phi_b = 0; %transport property at point a
F = rho*u; %flux 
D = K/dx; %diffusion 
fvm_x = dx/2:dx:(L-(dx/2)); %nodal distance values used for FVM
x = zeros(n+2,1); %creating matrix for x distance(m)
x(2,1) = dx/2; %second distance is equal to dx/2 which is first node point used in FVM
x(end) = L; %last element of x must be its length
for i = 3:n+1 %for loop for making the array x with nodal distances with A and B points
    x(i,1) = x(i-1,1) + dx; %array of nodal points distance values
end
ap = zeros(n,1); %preallocating array for ap coefficients
aw = zeros(n,1); %preallocating array for aw coefficients
ae = zeros(n,1); %preallocating array for ae coefficients
su = zeros(n,1); %preallocating array for su coefficients
sp = zeros(n,1); %preallocating array for sp coefficients
for i = 2:n %for loop for making array for aw coefficients
    aw(i,1) = D + (F/2); %aw coefficient value
end
for i = 1:n-1 %for loop for making array for ae coefficients
    ae(i,1) = D - (F/2); %ae coefficient value
end
for i = 2:n-1 %for loop for making array for ap coefficients
    ap(i,1) = (D + (F/2)) + (D - (F/2)); %ap coefficient value
end
for i = 1:n %for loop for making arrays for ap , su & sp coefficients
    if i==1 %if condition for node 1
        su(i,1) = (2*D + F)*phi_a;  %su coefficient value
        sp(i,1) = -(2*D + F);  %sp coefficient value
        ap(i,1) = (D - (F/2)) + (2*D + F);  %ap coefficient value
    elseif i==n %elseif condition for node n
        su(i,1) = (2*D - F)*phi_b;  %su coefficient value
        sp(i,1) = -(2*D - F);  %sp coefficient value
        ap(i,1) = (D + (F/2)) + (2*D - F);  %ap coefficient value
    end
end
%solution with TDMA
%a, b, c are the column vectors for the compressed tridiagonal matrix, d is the right vector and x is the output which is solution
S = zeros(n,n); %preallocating array for solution
X = zeros(n,1); %preallocating array for solution
B = zeros(n,1); %preallocating array for solution
phi = zeros(n+2,1); %preallocating array for solution
a = zeros(n-1,1); %preallocating array for TDMA
b = zeros(n,1); %preallocating array for TDMA
c = zeros(n-1,1); %preallocating array for TDMA
d = zeros(n,1); %preallocating array for TDMA
phi(1) = phi_a; %first element is  phi_a
phi(end) = phi_b; %last element is phi_b
for i = 1:n %for loop for making arrays S & B
    if i==1 %if condition for node 1
        S(i,i) = (D - (F/2)) + (2*D + F); %ap value at node 1
        b(i,1) = (D - (F/2)) + (2*D + F); %ap value at node 1
        B(i,1) = (2*D + F)*phi_a; %su value at node 1
        d(i,1) = (2*D + F)*phi_a; %su value at node 1
    elseif i==n
        S(i,i) = (D + (F/2)) + (2*D - F); %ap value at node n
        b(i,1) = (D + (F/2)) + (2*D - F); %ap value at node n
        B(i,1) = (2*D - F)*phi_b; %su value at node n
        d(i,1) = (2*D - F)*phi_b; %su value at node n
    end
end
for i = 2:n-1 %for loop for making array S 
    S(i,i) = (D + (F/2)) + (D - (F/2)); %ap value at nodes 2 to n-1
    b(i,1) = (D + (F/2)) + (D - (F/2)); %ap value at nodes 2 to n-1
end
for i = 1:n-1 %for loop for making array S 
    S(i,i+1) = -(D - (F/2)); %ae value from node 1 to n-1
    c(i,1) = -(D - (F/2)); %ae value from node 1 to n-1
    S(i+1,i) = -(D + (F/2)); %aw value from node 2 to n
    a(i,1) = -(D + (F/2)); %aw value from node 2 to n
end
N = length(d); %N is the number of rows
c(1) = c(1) / b(1); %Division by zero risk
d(1) = d(1) / b(1); %Division by zero risk
for i = 2:N-1
    temp = b(i) - a(i) * c(i-1);
    c(i) = c(i) / temp;
    d(i) = (d(i) - a(i) * d(i-1))/temp;
end
%back substitution
d(N) = (d(N) - a(N-1) * d(N-1))/( b(N) - a(N-1) * c(N-1));
X(N,1) = d(N);
for i = N-1:-1:1
    X(i,1) = d(i,1) - c(i,1) * X(i + 1,1);
end
for i = 1:n
    phi(i+1,1) = X(i,1);
end
figure (1)
plot(x,phi,'*')
title('Transport property Plot for 1D steady state conduction & convection')
xlabel('X [m]')
ylabel('Transport property [phi]')