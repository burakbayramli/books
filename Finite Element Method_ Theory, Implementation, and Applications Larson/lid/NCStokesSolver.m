function NCStokesSolver()
fd = @(p) -min(min(min(1+p(:,2),1-p(:,2)),1+p(:,1)),1-p(:,1));
fh = @(p) ones(size(p,1),1);
[p,t] = distmesh( fd, fh, 0.05, [-1,-1;1,1], [-1,-1;-1,1;1,-1;1,1] );
#[p,e,t]=initmesh('squareg');  
t2e=Tri2Edge(p,t); % triangle-to-edge adjacency
nt=size(t,2); % number of triangles
ne=max(t2e(:)); % number of edges
[A11,B1,B2,areas]=NCAssembler(p,t2e,t); % assemble
nu=0.1; % viscosity parameter
LHS=[nu*A11 sparse(ne,ne) B1';
     sparse(ne,ne) nu*A11 B2';
     B1 B2 sparse(nt,nt)]; % LHS matrix
rhs=zeros(2*ne+nt,1); % RHS vector
last=[zeros(2*ne,1); areas]; % last row and column
LHS=[LHS last; last' 0];
rhs=[rhs; 0];
[xmid,ymid,edges] = EdgeMidPoints(p,t2e,t);
fixed=[]; % fixed nodes
gvals=[]; % nodal values of g
for i=1:length(edges) % loop over edges
n=edges(i); % edge (ie. node) number
x=xmid(i); % x-coordinate of edge mid-point
y=ymid(i); % y-
if (x<-0.99 | x>0.99 | y<-0.99 | y>0.99) % boundary
fixed=[fixed; n; n+ne]; % fix velocity nodes
u=0; v=0; % bc values
if (y>0.99), u=1; end % u=1,v=0 on lid
gvals=[gvals; u; v];
end
end

neq=2*ne+nt+1; % number of equations
free=setdiff([1:neq],fixed);
rhs=rhs(free)-LHS(free,fixed)*gvals; % shrink vector
LHS=LHS(free,free); % shrink matrix
sol=zeros(neq,1); % allocate solution
sol(fixed)=gvals; % insert no-slip values
sol(free)=LHS\rhs; % solve linear system

U=sol(1:ne); V=sol(1+ne:2*ne); P=sol(2*ne+1:2*ne+nt);
figure(1), pdesurf(p,t,P')
figure(2), quiver(xmid,ymid,U',V')

