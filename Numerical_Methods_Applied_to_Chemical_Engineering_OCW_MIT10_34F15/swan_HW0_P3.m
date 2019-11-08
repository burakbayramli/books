function swan_HW0_P3
%Problem based on 'A colloidal quantum dot spectrometer' by Bao and Bawendi
%Nature 523:67, 2015
% Created August 3, 2015 Jim Swan

% Part 1
% Plot the tranmission spectra 
n_f = 30; %value provided in problem statement
n_lambda = 100; %value provided in problem statement

% Build transmission spectra matrix
A = Construct_A( n_f, n_lambda );

% Create plot
figure;
plot( linspace( 400, 600, n_lambda ), A' );
xlabel( 'lambda (nm)' );
ylabel( 'T(lambda)' );


% Part 3
% Find A for values of n_lambda = n_f = n from 2 to 20
% Plot the condition number, norm(A) and norm(inv(A))

% initialize storage variables
condA = zeros(1,19);
normAinv = zeros(1,19);
normA = zeros(1,19);

for n = 2:20
    % build the transmission spectrum
    A = Construct_A( n, n );
    
    condA( n-1 ) = cond( A ); %find the condition number
    normAinv( n-1 ) = norm( inv( A ) ); %find the norm of the inverse
    normA( n-1 ) = norm( A ); %find the norm
    
end; %end loop through sizes

% Plot the results
figure;
semilogy( 2:20, condA, 'ko' );
hold on;
semilogy( 2:20, normA, 'bo' );
semilogy( 2:20, normAinv, 'ro' );
hold off;
legend( 'cond(A)', 'norm(A)', 'norm(A^-1)', 'Location', 'NorthWest' );
xlabel('n');
xlim( [ 2 20 ] );

% Plot the difference between cond(A) and ||A||*||A^-1||
figure;
semilogy( 2:20, abs( ( condA - normA .* normAinv ) ./ condA ), 'ko' )
xlabel( 'n' );
ylabel( '| cond(A)-|A| |A^{-1}| | / cond(A)' );
xlim( [ 2 20 ] );

% Part 4
% The error bound is shown to be ||dPhi||/||Phi|| = 10^-3*cond(A) (see
% solution for details). Plot n vs error bound.
figure;
semilogy( 2:20, 10^-3 * condA, 'ko' );
xlabel( 'n' );
ylabel( 'Upper bound on relative error in \Phi due to error in J via 2-norm' );
xlim( [ 2 20 ] );

% Part 5
% Upper bound of the relative error in Phi in terms of the relative error
% in the transmission spectra (see solutions for details). Plot n vs error
% bound
figure;
semilogy( 2:20, 10^-3 * condA ./ ( 1 - 10^-3 * condA ) ./ ( condA < 10^3 ), 'ko' );
xlabel( 'n' );
ylabel( 'Upper bound on relative error in \Phi due to error in T via inf-norm' );
xlim( [ 2 20 ] );

end %end main function
        
function res = Construct_A( n_f, n_lambda )
% function to create the transmission spectra 
% inputs:   n_f - scalar, number of films in the array
%           n_lambda - scalar, number of wavelengths at which the incident  
%               light will be sampled  
% outputs:  res - matrix (n_f x n_lambda), simulated transmission spectrum

    w = 50; %specified parameter
    s = @(i) 400 + 200 * ( i - 1 ) / ( n_f - 1 ); %parameter, function of number of films
    T = @(i, lambda) 1 / ( 1 + exp( -( lambda - s( i ) ) / w ) ); %function for transmission spectrum

    lambda = linspace( 400, 600, n_lambda ); %create vector of lambda values, evenly spaced

    res = zeros(n_f, n_lambda); %initialize res
    
    for i = 1:n_f

        for j = 1:n_lambda

            res( i, j ) = T( i, lambda( j ) ); %populate res based on transmission spectrum
            
        end; %end loop through columns
        
    end; %end loop through rows
    
end %end function statement
        
        
        
