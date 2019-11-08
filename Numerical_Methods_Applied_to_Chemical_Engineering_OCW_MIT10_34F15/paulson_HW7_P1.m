function paulson_HW7_P1
    %Define the units
    D = 6e-6; %cm^2/s
    v0 = 0.5; % 1/s
    H = 1; %cm
    L = sqrt(D/v0);
    W = 1; %cm, width of patch
    
    %Test values of R and B until we show convergence
    R = 2*W/L+5:W/L:(10*W/L);
    B = H/L*0.01:0.01 * H/L:0.1 * H/L;
    
    %Compute flux for various B values
    dx = 9*W/L / 300;
    dy = 0.25;
    fluxFV_B= zeros(length(B),1);
    for i = 1:length(B)
        [X_FV , Y_FV , C_FV, fluxFV_B(i)] = FV_medical_implant(9*W/L,B(i),dx,dy);
    end
      
    % Plot FV flux versus B 
    figure
    plot(B / (H/L), fluxFV_B, 's')
    xlabel('B / H')
    ylabel('F/(D C_{eq})')
    set(gcf,'color','w')
    
    %Compute flux for various R values
    dx = 1;
    dy = 0.25;
    fluxFV_R= zeros(length(R),1);
    for i = 1:length(R)
        [X_FV , Y_FV , C_FV, fluxFV_R(i)] = FV_medical_implant(R(i),0.1*H/L,dx,dy);
    end
      
    % Plot FV flux versus B 
    figure
    plot(R / (W/L), fluxFV_R, 's')
    xlabel('R / W')
    ylabel('F/(D C_{eq})')
    set(gcf,'color','w')
    
    %Define the dimensionless width and height of the simulation cell
    R = 2500;
    B = 40;
    % Compute flux for various dx, dy values
    N = 200:200:1000';
    dx = zeros(length(N),1);
    dy = zeros(length(N),1);
    fluxFV = zeros(length(N),1);
    for i = 1:length(N)
        dx(i) = R/N(i);
        dy(i) = B/N(i);
        [X_FV , Y_FV , C_FV, fluxFV(i)] = FV_medical_implant(R,B,dx(i),dy(i));
    end



    % Plot FV flux versus dx (semilog for x axis)
    figure
    semilogx(N.*N, fluxFV, 's')
    xlabel('Number of grid points')
    ylabel('F/(D C_{eq})')
    set(gcf,'color','w')

    % Plot the solution using pcolor and eliminate any grid lines
    figure
    pcolor( X_FV, Y_FV, C_FV  );
    shading flat;
    ylabel( 'y/L' );
    xlabel( 'x/L' );
    title( 'C/C_{eq}' )
    colorbar;
    set(gcf,'color','w')

    save('HW7_plot_data.mat')
end

function [X , Y , C, flux] = FV_medical_implant(R,B,dx,dy)

    % Define the problem parameter: the Peclet number
    D = 6e-6; %cm^2/s
    v0 = 0.5; % 1/s
    H = 1; %cm
    L = sqrt(D/v0);
    W = 1; %cm
    % Define the dimensionless velocity field
    v = @( y ) y .* ( y < H/L ) + ( y >= H/L );

    % Define the values of x and y at each node
    x = ( 0:dx:R-dx ) + dx/2;
    y = ( 0:dy:B-dy ) + dy/2;

    % Find the index in x-direction where the implant starts
    % implant_start = 1 / dx;
    [~, implant_start] = min(abs(x-((W/L)+dx/2)));

    % Find the index in x-direction where the implant ends
    % implant_end = 2 / dx;
    [~, implant_end] = min(abs(x-((2*W/L)-dx/2)));

    % Keep track of the indices of the solution associated with the location of
    % the implant
    implant_indices = zeros(1, implant_end - implant_start + 1);
    implant_ctr = 0;
    % Preload a list of vectors to represent sparse matrix in [row,col,val]
    % format. We choose 5*Nx*Ny as maximum number of non-zero elements as we
    % know the bandwidth is 5. nz represents our non-zero element counter
    nz = 0;
    rowA = zeros( 5 * length( x ) * length ( y ), 1);
    colA = zeros( 5 * length( x ) * length ( y ), 1);
    valA = zeros( 5 * length( x ) * length ( y ), 1);
    
    b_ctr = 0;
    rowb = zeros(1, length(x) * length(y));
    colb = zeros(1, length(x) * length(y));
    valb = zeros(1, length(x) * length(y));

    tic
    % Loop over all nodes in x and y and write a linear equation for each node
    for i = 1:length( x )

        for j = 1:length( y )

            % Write the composite index using the normal order of points.  This
            % ordered moves along y first and then along x
            ij = j + ( i - 1 ) * length( y );
    %         ij = i + ( j - 1 ) * length( x );

            % Write the coefficients associated with equations for each of the
            % points on the boundary and in the domain.
            if ( ( i == 1 ) && ( j == 1 ) )
            % Left-bottom corner, c = 0

    %             A( ij, ij ) = -1 / dx^2 - 1 / dy^2 -  v( y( j ) ) / dx;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = -1 / dx^2 - 1 / dy^2 -  v( y( j ) ) / dx;

    %             A( ij, ij + 1 ) = 1 / dy^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + 1;
                valA( nz ) = 1 / dy^2;

    %             A( ij, ij + length( y ) ) = 1 / dx^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + length( y );
                valA( nz ) = 1 / dx^2;

    %             b( ij ) = 0;

            elseif ( ( i == 1 ) && ( j == length( y ) ) )
            % Left-top corner, c = 0        

    %             A( ij, ij ) = -1 / dx^2 - 1 / dy^2 -  v( y( j ) ) / dx;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = -1 / dx^2 - 1 / dy^2 -  v( y( j ) ) / dx;

    %             A( ij, ij - 1 ) = 1 / dy^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij - 1;
                valA( nz ) = 1 / dy^2;

    %             A( ij, ij + length( y ) ) = 1 / dx^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + length( y ) ;
                valA( nz ) = 1 / dx^2;

    %             b( ij ) = 0;

            elseif ( ( i == length( x ) ) && ( j == 1 ) )
            % Right-bottom corner , c = 0  

    %             A( ij, ij ) = 1;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = 1;

    %             b( ij ) = 0;

            elseif ( ( i == length( x ) ) && ( j == length( y ) ) )
            % Right-top corner, c = 0

    %             A( ij, ij ) = 1;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = 1;

    %             b( ij ) = 0;

            elseif ( ( ( i < implant_start ) || ( i > implant_end ) ) && ( j == 1 ) )
            % Left of right of implant, bottom boundary, no flux   

    %             A( ij, ij ) = -2 / dx^2 - 1 / dy^2 -  v( y( j ) ) / dx;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = -2 / dx^2 - 1 / dy^2 -  v( y( j ) ) / dx;

    %             A( ij, ij + 1 ) = 1 / dy^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + 1;
                valA( nz ) = 1 / dy^2;

    %             A( ij, ij + length( y ) ) = 1 / dx^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + length( y );
                valA( nz ) = 1 / dx^2;

    %             A( ij, ij - length( y ) ) = 1 / dx^2 +  v( y( j ) ) / dx;            
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij - length( y ) ;
                valA( nz ) = 1 / dx^2 +  v( y( j ) ) / dx;

    %             b( ij ) = 0;

            elseif ( j == 1 )
            % implant on bottom boundary, c = 1
                implant_ctr = implant_ctr +1;
                implant_indices(implant_ctr) = ij;               

    %             A( ij, ij ) = 1;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = 1;

    %             b( ij ) = 1;
                b_ctr = b_ctr + 1;
                rowb(b_ctr) = ij;
                colb(b_ctr) = 1;
                valb(b_ctr) = 1;
                

            elseif ( j == length( y ) )
            % Top boundary, c = 0

    %             A( ij, ij ) = -2 / dx^2 - 1 / dy^2 -  v( y( j ) ) / dx;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = -2 / dx^2 - 1 / dy^2 -  v( y( j ) ) / dx;

    %             A( ij, ij - 1 ) = 1 / dy^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij - 1;
                valA( nz ) = 1 / dy^2;

    %             A( ij, ij + length( y ) ) = 1 / dx^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + length( y );
                valA( nz ) = 1 / dx^2;

    %             A( ij, ij - length( y ) ) = 1 / dx^2 +  v( y( j ) ) / dx;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij - length( y );
                valA( nz ) = 1 / dx^2 +  v( y( j ) ) / dx;

    %             b( ij ) = 0;

            elseif ( i == 1 )
            % Left boundary, c = 0

    %             A( ij, ij ) = -1 / dx^2 - 2 / dy^2 -  v( y( j ) ) / dx;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = -1 / dx^2 - 2 / dy^2 -  v( y( j ) ) / dx;

    %             A( ij, ij + 1 ) = 1 / dy^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + 1;
                valA( nz ) = 1 / dy^2;

    %             A( ij, ij - 1 ) = 1 / dy^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij - 1;
                valA( nz ) = 1 / dy^2;

    %             A( ij, ij + length( y ) ) = 1 / dx^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + length( y );
                valA( nz ) = 1 / dx^2;

    %             b( ij ) = 0;            

            elseif ( i == length( x ) )
            % Right boundary, c = 0

    %             A( ij, ij ) = 1;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = 1;

    %             b( ij ) = 0;

            else
            % The rest of the domain: d^2 c / dx^2 + d^2 c / dy^2 = Pe v d c / dx
            % Use second order in diffusion, first order in
            % convection with backward/upwind differnce

    %             A( ij, ij ) = -2 / dx^2 - 2 / dy^2 -  v( y( j ) ) / dx;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij;
                valA( nz ) = -2 / dx^2 - 2 / dy^2 -  v( y( j ) ) / dx; % Cij

    %             A( ij, ij + 1 ) = 1 / dy^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + 1;
                valA( nz ) = 1 / dy^2;  %Ci,j+1

    %             A( ij, ij - 1 ) = 1 / dy^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij - 1;
                valA( nz ) = 1 / dy^2;

    %             A( ij, ij + length( y ) ) = 1 / dx^2;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij + length( y );
                valA( nz ) = 1 / dx^2;

    %             A( ij, ij - length( y ) ) = 1 / dx^2 +  v( y( j ) ) / dx;
                nz = nz + 1;
                rowA( nz ) = ij;
                colA( nz ) = ij - length( y );
                valA( nz ) = 1 / dx^2 +  v( y( j ) ) / dx;

                % This is centered difference in convection and introduces
                % problems with stability.  Don't do this.
    %             A( ij, ij ) = -2 / dx^2 - 2 / dy^2;
    %             A( ij, ij + 1 ) = 1 / dy^2;
    %             A( ij, ij - 1 ) = 1 / dy^2;
    %             A( ij, ij + length( y ) ) = 1 / dx^2 -  v( y( j ) ) / ( 2 * dx );
    %             A( ij, ij - length( y ) ) = 1 / dx^2 +  v( y( j ) ) / ( 2 * dx );

    %             b( ij ) = 0;

            end;

        end;

    end;
    toc

    % Make sparse A and b
    tic
    A = sparse( rowA(1:nz), colA(1:nz), valA(1:nz), length( x ) * length( y ), length( x ) * length( y ) );
    b = sparse( rowb(1:b_ctr), colb(1:b_ctr), valb(1:b_ctr), length( x ) * length( y ), 1 );
    toc

    % Solve the system of linear equations
    tic
    c = A \ b;
    toc

    % Calculate the flux using a first order approximation of the derivative
    % and the trapezoidal rule.
    c = full( c );
    flux = trapz( x( implant_start:implant_end ), c( implant_indices(1:implant_ctr) ) - c( implant_indices(1:implant_ctr) + 1 ) ) / dy;

    % Reshape the solution vector to be a matrix for plotting
    C = reshape( c, [ length( y ), length( x ) ] );
    [ X, Y ] = meshgrid( x, y );

end