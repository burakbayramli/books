% pde_ex1_bound.m
% This boundary m-file implements the boundary
% conditions,
% BC 1 : Dirichlet u = 0
% BC 2 : Dirichlet u = 0
% BC 3 : Dirichlet u = 0
% BC 4 : zero-flux Neuman
% BC 5 : Dirichlet u = 1
% BC 6 : zero-flux Neuman
%
% Kenneth J. Beers
% MIT Department of Chemical Engineering

function [q_e, g_e, h_e, r_e] = pde_ex1_bound(p,e,u,time);

N_f = 1;  % number of fields
ne = size(e,2);  % number of edges

% allocate memory to store output matrices
q_e = zeros(N_f^2, ne);
g_e = zeros(N_f, ne);
h_e = zeros(N_f^2, 2*ne);
r_e = zeros(N_f, 2*ne);

% Here, we have two types of BCs that we enforce,
% Dirichlet and zero-flux Neumann. Note that the
% pde toolkit can treat as well more general, mixed
% BC, but here we consider only these two (most common)
% examples.

% Dirichlet BC, written as
%  Hu = r
% where H is Nf x Nf matrix, u is Nf x 1 column vector
% of the local field values, and r is Nf x 1 column
% vector of the left-hand side.
%
% Here, we store the values of the known field values
% along the Dirichlet boundary sections. These could
% be functions of position as well.
%
% zero-flux BC, written as
% dot(n,C*grad(u)) + Q*u = g
% where n is outward unit normal vector. C merely
% extracts out the correct gradient values for
% a multi-field problem to compare with the
% corresponding rows of Q*u and g.

% We now store this data for our BC's in a convenient
% format that make sense based on equations for BCs
% (unlike format used in the output arguments)

num_edges = 6;
% BC 1 - Dirichlet u = 0
EDGE(1).type = 0;  % Dirichlet BC
EDGE(1).H = eye(N_f);
EDGE(1).r = 0;
% BC 2 - Dirichlet u = 0
EDGE(2).type = 0;
EDGE(2).H = eye(N_f);
EDGE(2).r = 0;
% BC 3 - Dirichlet u = 0
EDGE(3).type = 0;
EDGE(3).H = eye(N_f);
EDGE(3).r = 0;
% BC 4 - no-flux Neuman BC
EDGE(4).type = 1;
EDGE(4).Q = zeros(N_f,N_f);
EDGE(4).g = zeros(N_f,1);
% BC 5 - Dirichlet u = 1
EDGE(5).type = 0;
EDGE(5).H = eye(N_f);
EDGE(5).r = 1;
% BC 6 - no-flux Neuman BC
EDGE(6).type = 1;
EDGE(6).Q = zeros(N_f,N_f);
EDGE(6).g = zeros(N_f,1);
% Finally, for each of the Dirichlet BC, extract from
% H and Q the vectors of the matrix values, in column
% oriented form, as required by pdebound output format
for k=1:num_edges
    if(EDGE(k).type == 0)  % Dirichlet BC
        EDGE(k).h = zeros(N_f^2,1);
        count = 0;
        for n=1:N_f  % sum over columns
            for m=1:N_f % sum over rows
                count = count+1;
                EDGE(k).h(count) = EDGE(k).H(m,n);
            end
        end
    else(EDGE(k).type == 1)  % Neumann BC
        EDGE(k).q = zeros(N_f^2,1);
        count = 0;
        for n=1:N_f  % sum over columns
            for m=1:N_f % sum over rows
                count = count + 1;
                EDGE(k).q(count) = EDGE(k).Q(m,n);
            end
        end
    end
end

% now iterate through each edge, and return
% BC values appropriate for the section that
% the boundary is on.
for k=1:ne  % for each edge
    % extract info about current edge
    edge = e(:,k);
    % get boundary section of this edge
    edge_label = edge(5);
    % If this edge is a Dirichlet BC, extract
    % appropriate values of h and r.
    if(EDGE(edge_label).type == 0)  % if Dirichlet BC
        % find BC parameters for this edge
        h = EDGE(edge_label).h;
        r = EDGE(edge_label).r;
        % record once in column k of h_e for value
        % of BCs at start of the edge
        h_e(:,k) = h;
        r_e(:,k) = r;
        % record again in column ne+k for value of
        % BCs at end of the edge. We record these twice
        % to allow us to use variation in BC value
        % over the edge if we wish.
        h_e(:,ne+k) = h;
        r_e(:,ne+k) = r;
        
    else  % else if it is a Neuman BC
        % extract and record BC parameters for this
        % edge
        q_e(:,k) = EDGE(edge_label).q;
        g_e(:,k) = EDGE(edge_label).g;
    end    
end


return;
