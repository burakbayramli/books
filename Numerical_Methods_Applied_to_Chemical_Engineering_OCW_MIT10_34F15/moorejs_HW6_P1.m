function moorejs_HW4_P1
%Written by: David Couling, 10/06/09
%Modified by: Jason Moore on Oct 6, 2010
%Call moorejs_HW4_P1_entropy for each new value of M_max.  Find value of
%entropy and minimum value of M_max for converged solution to 1% tolerance

%Create M_max vector
M_max = 1:100;

%Initialize vector to store entropy values
S = zeros(length(M_max));

%Define I, V3, T
I = 1.2e-45;    %kg-m^2
V3 = 0.81e-20;  %J/particle
T = 300;        %K

%for loop to calculate entropy for each value of M_max
for i = 1:length(M_max)
    S(i) = moorejs_HW4_P1_entropy(I,V3,T,M_max(i));
end

figure
plot(M_max,S(:,1))
xlabel('M_{max}')
ylabel('Entropy (J/mol-K)')
title('Entropy convergence as a function of M_{max}')

%Determine optimal M_max.  Find value where (S(i+1)-S(i))/S(i) < 1e-2.
%Call the ratio S(i+1)-S(i)/S(i) "change"
for i = 1:(length(S)-1)
    change(i) = (S(i+1)-S(i))/S(i);
end
index = find(change<1e-2);
%The smallest index in this will be N_index_opt, and S_conv will be the
%converged value of S at this number of basis functions
N_index_opt = min(index)
S_conv = S(N_index_opt)

%//////////////////////////////////////////////////////////////////////////

function S = moorejs_HW4_P1_entropy(I,V3,T,M_max)
%Written by: David Couling, 10/06/09
%Modified by: Jason Moore on Oct 6, 2010
%Computes the entropy of a molecule based on solving the Schroedinger
%equation using Galerkin's method
%Inputs:    I is the moment of inertia of the molecule (kg-m^2)
%           V3 is the coefficient of the potential function (J/particle)
%           T is the temperature (K)
%           M_max is the number of basis functions to use
%Outputs:   S is the entropy of the molecule (J/mol-K)

%Define parameters
%Planck's constant
h = 6.626e-34;  %J-s
%Boltzmann constant
k_B = 1.38e-23; %J/K
%Avogadro's number
N_A = 6.022e23;

%Initialize matrix M.  We are going from -M_max to +M_max, so that is
%2*M_max+1 entries
M = zeros(2*M_max+1,2*M_max+1);

%i and j are indices of the matrix; the first entry of the matrix will be
%for (k,m) = (-M_max,-M_max), so i will = k + M_max + 1, and j will =
%m + M_max + 1
i=1;
j=1;
%Loop over k and m
for k = -M_max:1:M_max
    for m = -M_max:1:M_max
        %Diagonal elements of the matrix (every term but the cosine term)
        if k ==m
            M(i,j) = (-h^2/(8*pi*I)*-m^2+V3)*2*pi;
        
        %The cosine term, which is nonzero only if k-m = 3 or -3
        elseif (k-m)==3 || (k-m)==-3
            M(i,j) = -pi*V3;
        
        %Every other term is 0.
        else
            M(i,j) = 0;
        end
        
        %Increase j, since m will be increasing by 1 next time through the
        %loop
        j = j+1;
    end
    %Increase i, since k will be increasing by 1 next time through the loop
    i = i+1;
    
    %Reset j = 1, since we are starting over at the 1st column for each new
    %value of k
    j = 1;
end

%Find eigenvalues of matrix (this is 2*pi*En, so to get the energies,
%divide by 2pi)
En = eig(M)/(2*pi);

%Calculate U
U = N_A * sum(En.*exp(-En/k_B/T))/sum(exp(-En/k_B/T));

%Calculate A
A = -N_A*k_B*T*log(sum(exp(-En/k_B/T)));

%Calculate S
S = (U-A)/T;