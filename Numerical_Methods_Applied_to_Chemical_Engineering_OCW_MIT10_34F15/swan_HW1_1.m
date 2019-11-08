function swan_HW1_1
close all
clear
clc

% Generate matrices of different sizes and time linear solves at each size
M = 2:2:100;

% Test different values of M to determine when Matlab runs out of memory
%M = 1400;

%pre-allocate storage variables for plotting
BandedTime = zeros(50,1);
sparseTime = zeros(50,1);
fullTime = zeros(50,1);
pcgTime = zeros(50,1);

for i = 1:length( M )
    
    % N is defined in the problem statement as M^2
    N = M( i )^2;
    
    % For part b, define the vector a to be passed to the banded solver
    a = [-1/4, zeros(1,M(i)-2), -1/4, 1, -1/4, zeros(1,M(i)-2), -1/4];
    
    % b is definied in the problems statements as a vector of ones
    b = ones( N, 1 );
    
    % Sparse A
    A = spdiags( [ -b/4, -b/4, b, -b/4, -b/4 ], [ -M( i ), -1, 0, 1, M( i ) ], N, N );
    
    % Full A
    Afull = full(A);
        
    % Banded A
    tic
    BandedSolve(N,M(i),a,b);
    BandedTime(i) = toc;
    
    % Timing for sparse solve
    tic;
    A\b;
    sparseTime( i ) = toc;
    
    % Timing for full solve
    tic;
    Afull\b;
    fullTime( i ) = toc;
    
    % Timing for PCG solve
    tic;
    pcg( A, b, 1e-8, 1000 );
    pcgTime( i ) = toc;
    
end %end loop

% Plot the results
loglog( M, sparseTime, 'ko' );
hold on;
loglog(M, BandedTime, 'go');
loglog( M, fullTime, 'ro' );
loglog( M, pcgTime, 'bo' );
hold off;
xlabel( 'M' );
ylabel( 'Solution time (s)' );
legend( 'Sparse', 'Banded','Full', 'PCG' );

end % end function


function x = BandedSolve( N, M, a, b )
%This funciton performs Gaussian elimination and backwards substitution on
%a banded Toeplitz matrix
%INPUTS:
%N - size of matrix A
%M - bandwidth of matrix A
%a - vector of values for the non-zero elements of A of length 2M + 1
%b - vector of ones of length N
%OUTPUTS:
%x - a vector of size N which is the solution to Ax = b

%Initialize sparse storage variable for 'upper triangular matrix'
%Note that the problem statement requires that the storage matrix is of
%size N x 2M NOT N x 2M + 1, therefore one of the bands will be handled
%implicitly as the storage variable is populated
sparseA = zeros(N,2*M);

%First we populate the sparse storage variable with the -M+1 to M diagonal
%elements. The diagonals will be stored as columns
for j = 1:2*M
    if j <= M
        %for the columns before M, there are zeros in the top M-j rows
        sparseA(M-j+1:end,j) = a(j+1);
    else
        %for the columns after M, there are zeros in the bottom M-j rows
        sparseA(1:end+M-j,j) = a(j+1);
    end %end conditional to determine if we are above or below the main
    %diagonal band
end %end loop through columns

%The elimination steps proceed through the rows. There are section
%sections: rows 2 to M, rows M+1 to N-M+1, rows N-M+2 to N. No changes are
%required to row 1
for i = 2:M
    for j = 1:M-1
        if sparseA(i,j) == 0;
            %Do nothing. If the element is already zero, no elimination is
            %required. This step isn't required but it avoids the
            %computational cost of multiplying by zero
        else
            %In the full matrix, A_i,j would be eliminated used A_j,j.
            %This corresponds to element sparseA_j-M+i,M in our storage
            %scheme. 
            pivot = sparseA(i,j)/sparseA(j-M+i,M);
            %Update b as well as sparseA
            b(i) = b(i) - pivot*b(i-M+j);
            %Update M elements of sparseA using the pivot
            sparseA(i,j:M+j) = sparseA(i,j:M+j) - pivot*sparseA(j-M+i,M:2*M);
        end %end conditional statement
    end %end loop through columns
end %end loop through rows

for i = M+1:N-M+1
    %For rows M+1 to N, we need to eliminate the -M diagonal element
    pivot = a(1)/sparseA(i-M,M);
    %Update b accordingly
    b(i) = b(i) - pivot*b(i-M);
    %Update M-1 elements of A
    sparseA(i,1:M) = sparseA(i,1:M) - pivot*sparseA(i-M,M+1:2*M);
    %The rest of the eliminations can be done as above
    for j = 1:M-1
        if sparseA(i,j) == 0;
            %Do nothing (same as above)
        else
            pivot = sparseA(i,j)/sparseA(i-M+j,M);
            b(i) = b(i) - pivot*b(i-M+j);
            sparseA(i,j:j+M) = sparseA(i,j:j+M) - pivot*sparseA(i-M+j,M:2*M);
        end %end conditional statement
    end %end loop through columns
end %end loop through rows

%The last N-M+2 to N rows need to account for the fact that the number of
%non-zero elements in the row is decreasing
for i = N-M+2:N
    %Eliminate the -M diagonal element as above
    pivot = a(1)/sparseA(i-M,M);
    b(i) = b(i) - pivot*b(i-M);
    sparseA(i,1:M) = sparseA(i,1:M) - pivot*sparseA(i-M,M+1:2*M);
    for j = 1:M-1
        if sparseA(i,j) == 0;
            %do nothing
        else
            pivot = sparseA(i,j)/sparseA(i-M+j,M);
            b(i) = b(i) - pivot*b(i-M+j);
            %Test to see if M+N-i-j is greater than M. If true, there are
            %zeors in the pivot row which we will treat implicitly
            if M + N-i - j > M
                %use only M elements
                sparseA(i,j:M+j) = sparseA(i,j:M+j) - pivot*sparseA(i-M+j,M:2*M);
            else
                %number of elements is a funciton of j and the number of
                %rows that are remaining
                sparseA(i,j:M+N-i) = sparseA(i,j:M+N-i) - pivot*sparseA(i-M+j,M:2*M+N-i-j); 
            end 
        end      
    end
end

%lastly we need to perform backsubstitution to solve for x
%iniatize a variable for the answer
x = zeros(N,1);

%first solve for the last M elements of x
x(N) = b(N)/sparseA(end,M);
%continue moving up the x vector using the populated elements of x
for m = N-1:-1:N-M+1
    x(m) = (b(m) - sparseA(m,M+1:M+N-m)*x(m+1:end))/sparseA(m,M);
end
%find the final elements of x
for n = N-M:-1:1
    x(n) = (b(n) - sparseA(n,M+1:end)*x(n+1:n+M))/sparseA(n,M);
end


end