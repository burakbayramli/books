%% Uprime2.m

%% Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
%% Code to supplement lecture notes by Louise Olsen-Kettle:
%% Numerical solution of Partial Differential Equations (PDEs)
%% Webpage: http://espace.library.uq.edu.au/view/UQ:239427
%% ISBN: 978-1-74272-149-1

function Uprim = Uprime(t,U)
global s b n

A=zeros(n,n);

for i=1:n
    for j=1:n
        if j==i+1
           A(i,j) = -s*U(i);
        elseif j==i-1
           A(i,j) = s*U(i);
        end

    end
end

Uprim = A*U + b;

