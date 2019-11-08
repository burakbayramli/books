clear;

% Create a random matrix
A = rand( 10, 10 );


% Define the function and Jacobian
f = @( v, l ) [ A * v - l * v; -1/2 * norm( v ) ^ 2 + 1/2 ];
J = @( v, l ) [ ( A - l * eye( length(A) ) )' -v; -v', 0 ];


% Calculate the exact eigenspectrum
[ vexact, lexact ] = eig( A );
lexact = diag( lexact );

% Cleverly choose the initial guess to be near an exact eigenpair!
v = round( vexact( :, 1 ), 2 );
l = round( lexact( 1 ), 2 );
y = [ v; l ];


% Perform Newton-Raphson iterations stopping when the function norm
% criterion is satisfied
tol = 1e-8;
err = 1;
while ( err > tol )
    
    y = y - J( v, l ) \ f( v, l );
    
    v = y( 1:length( A ), 1 );
    l = y( length( A ) + 1 );
    
    err = norm( f( v, l ) );
    
end;


% Check that the computed eigenpair is near to the exact values 
disp( [ 'Relative error in the eigenvalue: ', num2str( norm( l - lexact( 1 ) ) / norm( lexact ) ) ] );
disp( [ 'Relative error in the eigenvector: ', num2str( norm( v - vexact( :, 1 ) ) ) ] );


