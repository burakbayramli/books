% function [x,y] = polygon1_geom(bs,s);
% --------------------
% This routine is a geometry function for the
% polygon, and demonstrates the syntax described
% by "doc pdegeom". We decribe the geometry by
% defining the boundary that encloses it. We describe
% the boundary as a number of non-overlapping
% curves described by this function.
function [x,y] = polygon1_geom(bs,s);

% specify the number of boundary sections
% and store the nodal positions
nbs = 6;  % number of boundary sections
% set (x,y) positions of each vertex
V = [0, 0; ... % node 1 
     1, 1; ... % node 2
     0, 2; ... % node 3
     1.5, 3; ... % node 4
     3, 3; ... % node 5
     3, 0;];  % node 6

% Now, construct a matrix such that each row
% corresponds to a section of the boundary.
% The first column contains the identifier
% of start-point vertex, and the second
% contains the identifier of the end-point
% vertex.
B = zeros(nbs,2);
for k=1:nbs  % for each boundary section
    n1 = k; % vertex at start of section
    % get vertex at end of section
    if(k == nbs)
        n2 = 1;
    else
        n2 = k+1;
    end
    % store values in row k of B
    B(k,:) = [n1, n2];
end

% Now, store a matrix D that has six columns,
% one for each boundary section. We parameterize
% the boundary curve by a contour parameter 0 <= s <= 1,
% such that the section # k starts at vertex BS(k,1)
% at s = 0 and ends at vertex BS(k,2) at s = 1.
%
% In rows 3 and 4, we specify what type of regions are
% found to the left and right of our curve, as we move
% along the boundary in increasing s. Here, since we move
% clockwise around the boundary, the left-hand region is
% outside of the domain and we set a 0 in row 3. The
% right-hand region is inside the computational domain,
% and thus we set row 4 equal to 1.
%
% In this problem, there is only one region of the domain,
% but in other problems, we could specify that the domain
% is comprised of regions 1, 2, 3, ... and we could use
% different PDE coefficients in each region. We then would
% have additional "internal" boundary sections that divide
% the overall domain into these non-overlapping subdomains.
% We would record the appropriate subdomain identifier
% 0, 1, 2, 3, ... for the left-hand side in row 3 and for
% the right hand side in row 4.
D = zeros(4,nbs);
for k=1:nbs  % for each section in the boundary
    % set s = 0 in row 1
    D(1,k) = 0;
    % set s = 0 in row 2
    D(2,k) = 1;
    % as left-hand side of boundary section is
    % outside of the domain, set 0 in row 3
    D(3,k) = 0;
    % as right-hand side of boundary section is
    % inside of the domain, set 1 in row 4
    D(4,k) = 1;
end

% Now, we are ready to return the appropriate
% output arguments

% if there are no input arguments, return the number
% of segments along the boundary
if(nargin == 0)
    x = nbs;
    y = 0;  % dummy value
    return;
end

% if there is only one input argument, bs, return
% the submatrix DS containing the columns of D
% specified in bs
if(nargin == 1)
    x = D(:,bs);
    y = 0;  % dummy value
    return;
end

% Else, if there are two input arguments, bs contains
% some number of boundary sections and s the values
% of s along the section at which x(s) and y(s) are
% to be computed.

% compute size of bs
[bs_numRows,bs_numCols] = size(bs);
% if bs is a scalar, than we are to compute
% the values of x(s) and y(s) at each value
% of s along the same curve. Thus, we expand
% bs to be the same size as s.
if((bs_numRows == 1)&&(bs_numCols == 1))
    bs = bs*(ones(size(s)));
elseif((bs_numRows ~= size(s,1))|(bs_numCols ~= size(s,2)))
     error('pdegeom: size(bs) ~= size(s)');
end
% Now, for each of these points along the
% specified boundary, we compute the corresponding
% positions x(s) and y(s).  Here s parameterizes
% the curve proportional to the arc length, For
% boundary sections that are not line segments, this
% is not true, and the pdearc1 function should
% be used to correct for this.
x = zeros(size(s)); y = zeros(size(s)); 
for m=1:size(s,1)
    for n=1:size(s,2)
        % record label of boundary section
        k = bs(m,n);
        % find the start and end vertex labels
        n1 = B(k,1);  n2 = B(k,2);
        % find the start and end positions
        x1 = V(n1,1);  x2 = V(n2,1);
        y1 = V(n1,2);  y2 = V(n2,2);
        % use linear interpolation to find the
        % value of x(s) and y(s)
        x(m,n) = (1-s(m,n))*x1 + s(m,n)*x2;
        y(m,n) = (1-s(m,n))*y1 + s(m,n)*y2;
    end
end

return;
