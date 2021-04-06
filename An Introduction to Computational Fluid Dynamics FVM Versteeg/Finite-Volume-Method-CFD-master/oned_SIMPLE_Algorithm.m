%code for 1D simple algorithm
%staggered grid for i,I
n = 4;
A = 1;
rho = 1; %density units must kg/m^3
d_co = 1; %ratio of coefficients A(i,J)/a(i,J)
u = zeros(n,1); %preallocating matrix for u velocities 
p_cor = zeros(n,1); %preallocating matrix for pressure correction
u(1,1) = 10; %inlet velocity units must be m/s
p_cor(end) = 0; %outlet pressure condition must be guage pressure in Pa
u_guess = [8; 11; 7]; %guess values of u
%Creating matrix to solve the pressure correction value
S = zeros(n-1,n-1); %preallocating array for solution
X = zeros(n-1,1); %preallocating array for solution
B = zeros(n-1,1); %preallocating array for solution
a = zeros(n-2,1); %preallocating array for TDMA
b = zeros(n-1,1); %preallocating array for TDMA
c = zeros(n-2,1); %preallocating array for TDMA
d = zeros(n-1,1); %preallocating array for TDMA
aw = rho*d_co*A; %aw value
ae = rho*d_co*A; %ae value
ap = aw+ae; %ap value
for I = 1:n-1
    if I == 1
        S(I,I) = ae;
        b(I,1) = ae;
        B(I,1) = -(rho*u_guess(I,1)*A) + ( rho*u(1,1)*A);
        d(I,1) = -(rho*u_guess(I,1)*A) + ( rho*u(1,1)*A);
    else
        S(I,I) = ap;
        b(I,1) = ap;
        for i = 1:n-2
            B(i+1,1) = (rho*u_guess(i,1)*A) - (rho*u_guess(i+1,1)*A);
            d(i+1,1) = (rho*u_guess(i,1)*A) - (rho*u_guess(i+1,1)*A);
            if I == n-1
                B(I,1) = ((rho*u_guess(i,1)*A) - (rho*u_guess(i+1,1)*A)) + p_cor(end);
                d(I,1) = ((rho*u_guess(i,1)*A) - (rho*u_guess(i+1,1)*A)) + p_cor(end);
            end
        end
    end
end
for I = 1:n-2
    S(I,I+1) = -(ae);
    c(I,1) = -(ae);
    S(I+1,I) = -(aw);
    a(I,1) = -(aw);
end
%TDMA 
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
for i = 1:n-1
    p_cor(i,1) = X(i,1);
end
%velocities
for i = 2:n
    for j = 1:n-2
        u(i,1) = u_guess(j,1)  + (d_co*(p_cor(j) - p_cor(j+1))); 
    end
end