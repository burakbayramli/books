% lattice_2D_vib.m
%
% This MATLAB program calculates the normal
% vibrational modes of a 2-D lattice of
% point masses where nearest neighbors
% are connected by simple Harmonic springs.
%
% K. Beers. MIT ChE. 10/03/2002

function iflag_main = lattice_2D_vib();

iflag_main = 0;

% First, we ask the user to set the size of the
% system.
N = input('Enter number of row(columns) of 2D lattice : ');
l_b = 1;  % equilibrium bond length
mass = 1;  % mass of each lattice "atom"

% Next, we set the size of the 2-D lattice
S = N^2;  % total number of lattice sites
F = 2*S;  % the total number of degrees of freedom

% We next set the vector of the spring constants linking
% each neighbor site in the system.
add_random = ...
    input('Add small random value to springs (0=no,1=yes) : ');
K_sp = set_spring_constants(add_random,N,S,F);

% Next, the user requests whether to calculate a few
% eigenvalues of largest magnitude, smallest magnitude,
% or near some input value, or to compute all eigenvalues
% using the QR method. In the latter case, the Hessian
% matrix must be converted to a full matrix, increasing
% greately the cost in memory.
disp(' ');
disp('There are two options for calculating the eigenvalues :');
disp('0 = use eigs() to compute only a few eigenvalues');
disp('1 = use eig() to compute all eigenvalues');
i_eig_routine = input('Use eigs (0) or eig (1) :');
if(~i_eig_routine)
    disp('Select which types of eigenvalues to compute : ');    
    disp('     1 = largest magnitude');
    disp('    -1 = smallest magnitude');
    disp('     2 = both ends');
    disp('     3 = search near target value');
    i_search = input('Enter search method : ');
    if(i_search==3)
        target = input('Enter target eigenvalue : ');
    end
    num_eig = input('Enter number of eigenvalues to calculate : ');
else
    disp('Hessian will be converted to full matrix format');
end

% We now store the positions of each lattice site
% at equilibrium in the minimum energy state.
q_min = zeros(F,1);
for i=1:N
    for j=1:N
        [k,kx,ky] = get_master_index(i,j,N);
        q_min(kx) = (i-1)*l_b;
        q_min(ky) = (j-1)*l_b;
    end
end

% Make a plot of the minimum energy lattice positions
figure;
hold on;
for i=1:N
    for j=1:N
        [k,kx,ky] = get_master_index(i,j,N);
        plot(q_min(kx),q_min(ky),'o');
    end
end

% We next compute the value of the potential energy and the
% potential energy gradient at the minimum energy state.
[U_min,grad_min] = calc_2D_lattice_energy(q_min,K_sp,l_b,N,S,F);

% We then use this function to approximate the Hessian using
% finite difference approximation.
% establish sparsity pattern of Hessian
H_sp = Hessian_sparsity_pattern(N,S,F);
% use finite difference approximation
Hessian = approx_Hessian_FD(q_min,grad_min,H_sp,K_sp,l_b,N,S,F);

% We now compute the normal modes and the frequencuies of each
% using eigenvalue analysis.
if(i_eig_routine)
    [V_eig,D_eig] = eig(full(Hessian));
else
    OPTS.isreal = 1;
    if(i_search == 1)
        [V_eig,D_eig] = eigs(Hessian,num_eig,'LM',OPTS);
    elseif(i_search == -1)
        [V_eig,D_eig] = eigs(Hessian,num_eig,'SM',OPTS);
    elseif(i_search == 2)
        [V_eig,D_eig] = eigs(Hessian,num_eig,'BE',OPTS);
    elseif(i_search == 3)
        [V_eig,D_eig] = eigs(Hessian,num_eig,target,OPTS);
    end
end

lambda_eig = diag(D_eig);
% make zero any negative values due to round-off
% error in computation of zero eigenvalues.
j_neg = find(lambda_eig < 0);
for k=1:length(j_neg)
    lambda_eig(j_neg(k)) = 0;
end
% compute angular frequencies
freq = sqrt(lambda_eig./mass);

% The results are now saved to an output file.
save lattice_2D_vib.mat;

iflag_main = 1;
return;



% ==============================================================
% get_master_index.m
%
% This function calculates the master index label of
% a site in the 2D lattice of NxN sites.
function [k,kx,ky] = get_master_index(i,j,N);

k = (i-1)*N+j;
kx = 2*k-1;
ky = 2*k;

return;


% ==============================================================
% set_spring_constants.m
% This MATLAB function computes the spring constants
% for each spring in the 2-D lattice of NxN sites.
% Currently, all spring constants are set to the
% same value.
function [K_sp,iflag] = ...
    set_spring_constants(add_random,N,S,F);
iflag = 0;

K_sp = spalloc(S,S,4*S);
K_val = 1; % common value of spring constant
for i=1:N
for j=1:N
    k = get_master_index(i,j,N);
    % site to left
    if(i>1)
        n = get_master_index(i-1,j,N);
    else
        n = get_master_index(N,j,N);
    end
    K_sp(k,n) = K_val; K_sp(n,k) = K_val;
    % site to right
    if(i<N)
        n = get_master_index(i+1,j,N);
    else
        n = get_master_index(1,j,N);
    end
    K_sp(k,n) = K_val; K_sp(n,k) = K_val;
    % site below
    if(j>1)
        n = get_master_index(i,j-1,N);
    else
        n = get_master_index(i,N,N);
    end
    K_sp(k,n) = K_val; K_sp(n,k) = K_val;
    % site above
    if(j<N)
        n = get_master_index(i,j+1,N);
    else
        n = get_master_index(i,1,N);
    end
    K_sp(k,n) = K_val; K_sp(n,k) = K_val;
end
end

% we now remove much of the degeneracy of the
% system by adding small random value to
% each spring constant.
if(add_random)
    random_factor = 0.01;
    for k=1:N
        % find non-zero spring constants
        n_list = find(K_sp(k,:));
        % add a small random amount (10% of K_val)
        for j=1:length(n_list)
            n = n_list(j);
            if(n > k)  % only consider each spring once
                K_new = K_val*(1 + random_factor*(rand-0.5));
                K_sp(k,n) = K_new; K_sp(n,k) = K_new;
            end
        end
    end
end
    
iflag = 1;
return;



% ==============================================================
% calc_2D_lattice_energy.m
%
% This MATLAB subourine calculates the potential energy and its
% gradient vector for a 2-D lattice where the neighboring
% sites are bonded by Harmonic springs. 
function [U,grad,iflag] = ...
    calc_2D_lattice_energy(q,K_sp,l_b,N,S,F);

iflag = 0;

% First, initialize total potential energy and
% gradient vector to zeros
U = 0;
grad = zeros(F,1);

% We now iterate over every site in the lattice.
for i=1:N
for j=1:N
    % extract position of lattice site
    [k,kx,ky] = get_master_index(i,j,N);
    x_k = q(kx);
    y_k = q(ky);
    % We now compute the forces on each site from
    % the four bonds attached to it (Note - this is
    % somewhat inefficient, as we will consider each
    % bond twice). The loss in efficiency is
    % compensated by the gain in transparency.
    
    % upper spring
    % extract position of neighbor lattice site's image
    if(j<N)
        [n,nx,ny] = get_master_index(i,j+1,N);
        x_n = q(nx);
        y_n = q(ny);        
    else  % need periodic boundary conditions
        [n,nx,ny] = get_master_index(i,1,N);
        x_n = q(nx);
        y_n = q(ny) + N*l_b;
    end
    % compute force in x and y direction from this
    % spring
    [U_sp,force_sp] = ...
        calc_spring_force(x_k,y_k,x_n,y_n,l_b,K_sp(k,n));
    % increment total potential energy
    if(n>k)
        U = U + U_sp;
    end    
    % update appropriate gradient values with the force
    % x-dir. gradient for site (i,j)
    grad(kx) = grad(kx) - force_sp(1);
    % y-dir. gradient for site (i,j)
    grad(ky) = grad(ky) - force_sp(2);

    % right spring
    % extract position of neighbor lattice site's image
    if(i<N)
        [n,nx,ny] = get_master_index(i+1,j,N);
        x_n = q(nx);
        y_n = q(ny);        
    else
        [n,nx,ny] = get_master_index(1,j,N);
        x_n = q(nx) + N*l_b;
        y_n = q(ny);
    end
    % compute force in x and y direction from this
    % spring
    [U_sp,force_sp] = ...
        calc_spring_force(x_k,y_k,x_n,y_n,l_b,K_sp(k,n));
    % increment total potential energy
    if(n>k)
        U = U + U_sp;
    end    
    % update appropriate gradient values with the force
    % x-dir. gradient for site (i,j)
    grad(kx) = grad(kx) - force_sp(1);
    % y-dir. gradient for site (i,j)
    grad(ky) = grad(ky) - force_sp(2);

    % lower spring
    % extract position of neighbor lattice site's image
    if(j>1)
        [n,nx,ny] = get_master_index(i,j-1,N);
        x_n = q(nx);
        y_n = q(ny);        
    else  % need periodic boundary conditions
        [n,nx,ny] = get_master_index(i,N,N);
        x_n = q(nx);
        y_n = q(ny) - N*l_b;
    end
    % compute force in x and y direction from this
    % spring
    [U_sp,force_sp] = ...
        calc_spring_force(x_k,y_k,x_n,y_n,l_b,K_sp(k,n));
    % increment total potential energy
    if(n>k)
        U = U + U_sp;
    end    
    % update appropriate gradient values with the force
    % x-dir. gradient for site (i,j)
    grad(kx) = grad(kx) - force_sp(1);
    % y-dir. gradient for site (i,j)
    grad(ky) = grad(ky) - force_sp(2);

    % left spring
    % extract position of neighbor lattice site's image
    if(i>1)
        [n,nx,ny] = get_master_index(i-1,j,N);
        x_n = q(nx);
        y_n = q(ny);        
    else
        [n,nx,ny] = get_master_index(N,j,N);
        x_n = q(nx) - N*l_b;
        y_n = q(ny);
    end
    % compute force in x and y direction from this
    % spring
    [U_sp,force_sp] = ...
        calc_spring_force(x_k,y_k,x_n,y_n,l_b,K_sp(k,n));
    % increment total potential energy
    if(n>k)
        U = U + U_sp;
    end    
    % update appropriate gradient values with the force
    % x-dir. gradient for site (i,j)
    grad(kx) = grad(kx) - force_sp(1);
    % y-dir. gradient for site (i,j)
    grad(ky) = grad(ky) - force_sp(2);

end
end

iflag = 1;
return;



% ==============================================================
% This function calculates for a single spring the energy and
% the spring force acting on the first (k) site.
function [U_sp,force_sp,iflag] = ...
    calc_spring_force(x_k,y_k,x_n,y_n,l_b,K_sp);
iflag = 0;

% compute potential energy of spring and increment
% total potential energy
dr = sqrt((x_k-x_n)^2 + (y_k-y_n)^2);
U_sp = 0.5*K_sp*(dr-l_b)^2;

% compute x and y direction forces acting on the neighbor
% site from this spring
force_sp = zeros(2,1);
force_sp(1) = -K_sp*(dr-l_b)*(x_k-x_n)/dr;
force_sp(2) = -K_sp*(dr-l_b)*(y_k-y_n)/dr;

iflag = 1;
return;



% ==============================================================
% approx_Hessian_FD.m
% This MATLAB function uses finite difference approximations
% to estimate the Hessian matrix.
function [Hessian,iflag] = ...
    approx_Hessian_FD(q_min,grad_min,H_sp,K_sp,l_b,N,S,F);

iflag = 0;

% First, allocate space as a sparse matrix for the Hessian.
nzH = nnz(H_sp);
Hessian = spalloc(F,F,nzH);

% We now displace each generalized coordinate by a
% small amount and recompute the gradient vector.
% A finite difference approximation then yields the
% values in the corresponding column of the Hessian.
epsilon = sqrt(eps);
for j=1:F
    q = q_min;
    q(j) = q_min(j) + epsilon;
    [U,grad] = calc_2D_lattice_energy(q,K_sp,l_b,N,S,F);
    col_vector = (grad - grad_min)./epsilon;
    i_list = find(H_sp(:,j));
    for k=1:length(i_list)
        Hessian(i_list(k),j) = col_vector(i_list(k));
    end
end

% Now, ensure that Hessian is symmetric
Hessian = (Hessian + Hessian')/2;

iflag = 1;
return;


% =============================================================
% Hessian_sparsity_pattern.m
% This MATLAB function sets the expected sparsity pattern of
% the Hessian matrix for the 2D lattice system.
%
function [H_sp,iflag] = Hessian_sparsity_pattern(N,S,F);
iflag = 0;

% allocate space as sparse matrix
H_sp = spalloc(F,F,6*F);
% for each lattice site
for i=1:N
for j=1:N
    [k,kx,ky] = get_master_index(i,j,N);

    % upper spring
    if(j<N)
        [n,nx,ny] = get_master_index(i,j+1,N);
    else  % need periodic boundary conditions
        [n,nx,ny] = get_master_index(i,1,N);
    end
    H_sp(kx,kx) = 1;  H_sp(ky,ky) = 1;
    H_sp(kx,ky) = 1;  H_sp(ky,kx) = 1;
    H_sp(kx,nx) = 1;  H_sp(nx,kx) = 1;
    H_sp(kx,ny) = 1;  H_sp(ny,kx) = 1;
    H_sp(ky,nx) = 1;  H_sp(nx,ky) = 1;
    H_sp(ky,ny) = 1;  H_sp(ky,ny) = 1;

    % right spring
    % extract position of neighbor lattice site's image
    if(i<N)
        [n,nx,ny] = get_master_index(i+1,j,N);
    else
        [n,nx,ny] = get_master_index(1,j,N);
    end
    H_sp(kx,kx) = 1;  H_sp(ky,ky) = 1;
    H_sp(kx,ky) = 1;  H_sp(ky,kx) = 1;
    H_sp(kx,nx) = 1;  H_sp(nx,kx) = 1;
    H_sp(kx,ny) = 1;  H_sp(ny,kx) = 1;
    H_sp(ky,nx) = 1;  H_sp(nx,ky) = 1;
    H_sp(ky,ny) = 1;  H_sp(ky,ny) = 1;


    % lower spring
    % extract position of neighbor lattice site's image
    if(j>1)
        [n,nx,ny] = get_master_index(i,j-1,N);
    else  % need periodic boundary conditions
        [n,nx,ny] = get_master_index(i,N,N);
    end
    H_sp(kx,kx) = 1;  H_sp(ky,ky) = 1;
    H_sp(kx,ky) = 1;  H_sp(ky,kx) = 1;
    H_sp(kx,nx) = 1;  H_sp(nx,kx) = 1;
    H_sp(kx,ny) = 1;  H_sp(ny,kx) = 1;
    H_sp(ky,nx) = 1;  H_sp(nx,ky) = 1;
    H_sp(ky,ny) = 1;  H_sp(ky,ny) = 1;

    % left spring
    % extract position of neighbor lattice site's image
    if(i>1)
        [n,nx,ny] = get_master_index(i-1,j,N);
    else
        [n,nx,ny] = get_master_index(N,j,N);
    end
    H_sp(kx,kx) = 1;  H_sp(ky,ky) = 1;
    H_sp(kx,ky) = 1;  H_sp(ky,kx) = 1;
    H_sp(kx,nx) = 1;  H_sp(nx,kx) = 1;
    H_sp(kx,ny) = 1;  H_sp(ny,kx) = 1;
    H_sp(ky,nx) = 1;  H_sp(nx,ky) = 1;
    H_sp(ky,ny) = 1;  H_sp(ky,ny) = 1;
   
end
end

% ensure symmetry
H_sp = (H_sp + H_sp')/2;

iflag = 1;
return;



