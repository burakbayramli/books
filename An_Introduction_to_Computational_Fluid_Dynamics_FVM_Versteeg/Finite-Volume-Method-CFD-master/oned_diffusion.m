%Code for 1d steady state heat conduction without heat generation using FVM
k = 1000; %thermal condcutivity of material units must be in W/m-K
A = 10*10^-3; %perpendicular area units must be m^2
l = 0.5; %length of 1D problem
n = 5; %node points
dx = l/n; %distance between points
Ta = 100; %temperature at A
Tb = 500; %temperature at B
fvm_x = dx/2:dx:(l-(dx/2)); %nodal distance values used for FVM
x = zeros(n+2,1); %creating matrix for x distance(m)
x(2,1) = dx/2; %second distance is equal to dx/2 which is first node point used in FVM
x(end) = l; %last element of x must be its length
for i = 3:n+1 %for loop for making the array x with nodal distances with A and B points
    x(i,1) = x(i-1,1) + dx; %array of nodal points distance values
end
ap = zeros(n,1); %preallocating array for ap coefficients
aw = zeros(n,1); %preallocating array for aw coefficients
ae = zeros(n,1); %preallocating array for ae coefficients
su = zeros(n,1); %preallocating array for su coefficients
sp = zeros(n,1); %preallocating array for sp coefficients
for i = 2:n %for loop for making array for aw coefficients
    aw(i,1) = (k*A)/dx; %aw coefficient value
end
for i = 1:n-1 %for loop for making array for ae coeffcients
    ae(i,1) = (k*A)/dx; %ae coefficient value
end
for i = 2:n-1 %for loop for making array for ap coeffcients
    ap(i,1) = (2*k*A)/dx; %ap coefficients value from 2 to n-1 nodes
end
for i = 1:n %for loop for making array for ap su & sp coeffcients
    if i==1 %if condition for node 1
        su(i,1) = ((2*k*A)/dx)*Ta; %su value at node 1
        sp(i,1) = -((2*k*A)/dx); %sp value at node 1
        ap(i,1) = (3*k*A)/dx; %ap value at node 1
    elseif i==n %elseif condition for node n
        su(i,1) = ((2*k*A)/dx)*Tb; %su value at node n
        sp(i,1) = -((2*k*A)/dx); %sp value at node n
        ap(i,1) = (3*k*A)/dx; %ap value at node n
    end
end
%solution with TDMA
%a, b, c are the column vectors for the compressed tridiagonal matrix, d is the right vector and x is the output which is solution
S = zeros(n,n); %preallocating array for solution
X = zeros(n,1); %preallocating array for solution
B = zeros(n,1); %preallocating array for solution
T = zeros(n+2,1); %preallocating array for solution
a = zeros(n-1,1); %preallocating array for TDMA
b = zeros(n,1); %preallocating array for TDMA
c = zeros(n-1,1); %preallocating array for TDMA
d = zeros(n,1); %preallocating array for TDMA
T(1) = Ta; %first element of array is Ta
T(end) = Tb; %last elment of array is Tb
for i = 1:n %for loop for making arrays S & B
    if i==1 %if condition for node 1
        S(i,i) = (3*k*A)/dx; %ap value at node 1
        b(i,1) = (3*k*A)/dx; %ap value at node 1
        B(i,1) = ((2*k*A)/dx)*Ta; %su value at node 1
        d(i,1) = ((2*k*A)/dx)*Ta; %su value at node 1
    elseif i==n
        S(i,i) = (3*k*A)/dx;%ap value at node n
        b(i,1) = (3*k*A)/dx; %ap value at node n
        B(i,1) = ((2*k*A)/dx)*Tb; %su value at node n
        d(i,1) = ((2*k*A)/dx)*Tb; %su value at node n
    end
end
for i = 2:n-1
    S(i,i) = (2*k*A)/dx; %ap value at nodes 2 to n-1
    b(i,1) = (2*k*A)/dx; %ap value at nodes 2 to n-1
end
for i = 1:n-1
    S(i,i+1) = -((k*A)/dx); %ae value from node 1 to n-1
    c(i,1) = -((k*A)/dx); %ae value from node 1 to n-1
    S(i+1,i) = -((k*A)/dx); %aw value from node 2 to n
    a(i,1) = -((k*A)/dx); %aw value from node 2 to n
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
    T(i+1,1) = X(i,1);
end
figure (1)
plot(x,T,'*')
title('Temeperature Plot for 1D steady state heat conduction without heatgeneration')
xlabel('X [m]')
ylabel('Temperature [°C]')