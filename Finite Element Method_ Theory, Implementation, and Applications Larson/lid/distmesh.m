function [ p, t, stat ] = distmesh( fd, fh, h0, bbox, p_fix, e_fix, it_max, fid, fit )
% DISTMESH 2D/3D Mesh generation using distance functions.
%
%   [ P, T, STAT ] = DISTMESH( FD, FH, H0, BBOX, P_FIX, E_FIX, IT_MAX, FID, FIT )
%
%   DistMesh is a simple surface (in 2D) and volume (in 3D) mesh gen-
%   eration algorithm using distance functions to define geometries.
%
%   FD is a function handle to the geometry description that should
%   take evaluation coordinates and points as input. For example fd =
%   @(p) sqrt(sum(p.^2,2)) - 1; specifies the distance function for a
%   unit circle (both function handles, string function names, and
%   anonymous functions are supported). Similar to FD, FH a function
%   describing the desired relative mesh size distribution. For
%   example fh = @(p) ones(size(p,1),1); specifies a uniform
%   distribution where FH evaluates to 1 at all points. H0 is a
%   numeric scalar specifying the initial edge lengths, and BBOX is a
%   2 by 2 in 2D (or 2 by 3 in 3D) bounding box of the domain
%   (enclosing the zero contour/level set of FD). P_FIX optionally
%   specifies a number of points that should always be present (fixed)
%   in the resulting mesh. E_FIX can be sets of edge vertex indices to
%   constrain, or alternatively a cell array with function handle to
%   call. IT_MAX sets the maximum number of grid generation iterations
%   allowed (default 1000). Finally, FID specifies a file identifies
%   for output (default 1 = terminal output), FIT is an optional
%   function to call every iteration to check for early termination.
%
%   The distmesh function returns the grid point vertices in P,
%   triangulated simplices in T, as well as an optional statistics
%   struct STAT including timings and convergence information.
%
%
%   Input:
%
%      FD:        Distance function d(x,y,(z))
%      FH:        Scaled edge length function h(x,y,(z))
%      H0:        Initial edge length
%      BBOX:      Bounding box [xmin,ymin,(zmin); xmax,ymax,(zmax)]
%      P_FIX:     Fixed node positions (N_P_FIX x 2/3)
%      E_FIX:     Constrained edges (N_E_FIX x 2)
%      IT_MAX:    Maximum number of iterations
%      FID:       Output file id number (default 1 = terminal)
%      FIT:       Optional iteration function call (default none)
%
%   Output:
%
%      P:         Grid vertex/node coordinates (N_P x 2/3)
%      T:         Triangle indices (N_T x 3)
%      STAT:      Mesh generation statistics (struct)
%
%
%   Example 1: (Uniform mesh on unit circle)
%      fd = @(p) sqrt(sum(p.^2,2)) - 1;
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.2, [-1,-1;1,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 2: (Uniform mesh on ellipse)
%      fd = @(p) p(:,1).^2/2^2 + p(:,2).^2/1^2 - 1;
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.2, [-2,-1;2,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 3: (Uniform mesh on unit square)
%      fd = @(p) -min(min(min(1+p(:,2),1-p(:,2)),1+p(:,1)),1-p(:,1));
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.2, [-1,-1;1,1], [-1,-1;-1,1;1,-1;1,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 4: (Uniform mesh on complex polygon)
%      pv = [-0.4 -0.5;0.4 -0.2;0.4 -0.7;1.5 -0.4;0.9 0.1;
%            1.6 0.8;0.5 0.5;0.2 1;0.1 0.4;-0.7 0.7;-0.4 -0.5];
%      fd = { 'l_dpolygon', [], pv };
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.1, [-1,-1; 2,1], pv );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 5: (Rectangle with circular hole, refined at circle boundary)
%      drectangle = @(p,x1,x2,y1,y2) -min(min(min(-y1+p(:,2),y2-p(:,2)),-x1+p(:,1)),x2-p(:,1));
%      fd = @(p) max( drectangle(p,-1,1,-1,1), -(sqrt(sum(p.^2,2))-0.5) );
%      fh = @(p) 0.05 + 0.3*(sqrt(sum(p.^2,2))-0.5);
%      [p,t] = distmesh( fd, fh, 0.05, [-1,-1;1,1], [-1,-1;-1,1;1,-1;1,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 6: (Square, with size function point and line sources)
%      dcircle = @(p,xc,yc,r) sqrt((p(:,1)-xc).^2+(p(:,2)-yc).^2)-r;
%      fd = @(p) -min(min(min(p(:,2),1-p(:,2)),p(:,1)),1-p(:,1));
%      dpolygon = @(p,v) feval('l_dpolygon',p,v);
%      fh = @(p) min(min(0.01+0.3*abs(dcircle(p,0,0,0)), ...
%                        0.025+0.3*abs(dpolygon(p,[0.3,0.7;0.7,0.5;0.3,0.7]))),0.15);
%      [p,t] = distmesh( fd, fh, 0.01, [0,0;1,1], [0,0;1,0;0,1;1,1] );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 7: (NACA0012 airfoil)
%      hlead = 0.01; htrail = 0.04; hmax = 2; circx = 2; circr = 4;
%      a = 0.12/0.2*[0.2969,-0.126,-0.3516,0.2843,-0.1036];
%      fd = @(p) max( dcircle(p,circx,0,circr), ...
%                     -((abs(p(:,2))-polyval([a(5:-1:2),0],p(:,1))).^2-a(1)^2*p(:,1)) );
%      fh = @(p) min(min(hlead+0.3*dcircle(p,0,0,0),htrail+0.3*dcircle(p,1,0,0)),hmax);
%
%      fixx = 1 - htrail*cumsum(1.3.^(0:4)');
%      fixy = a(1)*sqrt(fixx) + polyval([a(5:-1:2),0],fixx);
%      pfix = [[circx+[-1,1,0,0]*circr; 0,0,circr*[-1,1]]'; 0,0; 1,0; fixx,fixy; fixx,-fixy];
%      bbox = [circx-circr,-circr; circx+circr,circr];
%      h0   = min([hlead,htrail,hmax]);
%      [p,t] = distmesh( fd, fh, h0, bbox, pfix );
%      patch( 'vertices', p, 'faces', t, 'facecolor', [.9, .9, .9] )
%
%   Example 8: (Uniform mesh on unit sphere)
%      fd = @(p) sqrt(sum(p.^2,2)) - 1;
%      fh = @(p) ones(size(p,1),1);
%      [p,t] = distmesh( fd, fh, 0.2, [-1,-1,-1;1,1,1] );
%      f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
%      patch( 'vertices', p, 'faces', f, 'facecolor', [.9, .9, .9] )
%
%   Example 9: (Uniform mesh on unit cube)
%      fd = @(p) -min(min(min(min(min(p(:,3),1-p(:,3) ),p(:,2)),1-p(:,2)),p(:,1)),1-p(:,1));
%      fh = @(p) ones(size(p,1),1);
%      pfix = [-1,-1,-1;-1,1,-1;1,-1,-1;1,1,-1; -1,-1,1;-1,1,1;1,-1,1;1,1,1];
%      [p,t] = distmesh( fd, fh, 0.2, [-1,-1,-1;1,1,1], pfix );
%      f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
%      patch( 'vertices', p, 'faces', f, 'facecolor', [.9, .9, .9] ), view(3)
%
%   Example 10: (Uniform mesh on cylinder)
%      fd = @(p) -min(min(p(:,3),4-p(:,3)),1-sqrt(sum(p(:,1:2).^2,2)));
%      fh = @(p) ones(size(p,1),1);
%      pfix = [-1,-1,-1;-1,1,-1;1,-1,-1;1,1,-1; -1,-1,1;-1,1,1;1,-1,1;1,1,1];
%      [p,t] = distmesh( fd, fh, 0.5, [-1,-1,0;1,1,4], [] );
%      f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
%      patch( 'vertices', p, 'faces', f, 'facecolor', [.9, .9, .9] ), view(3)
%
%   See also DISTMESH_DEMO, DELAUNAY.

%   Copyright (C) 2004-2012 Per-Olof Persson, 2018 Precise Simulation Limited.
%   See COPYRIGHT.TXT for details.
if( ~(nargin || nargout) ),help distmesh, return, end

t0 = tic;
if( nargin<9 )
  fit = [];
end
if( nargin<8 )
  fid = 1;
end
if( nargin<7 )
  it_max = 1000;
end
if( nargin<6 )
  e_fix = [];
end
if( nargin<5 )
  p_fix = [];
end

%------------------------------------------------------------------------------%
% Initialization and meshing parameters.
%------------------------------------------------------------------------------%
IALG    = 2;              % Optimized algorithm selection.
IT_MIN  = 20;             % Minimum number of iterations.
IT_MINC = 50;             % Minimum number of iter. after which to call constraint function.
IT_PRT  = 25;             % Output every IT_PRT iterations.

N_RECV  = 2;              % Number of recovery iteration steps to move points outside back to boundary.
N_DCF   = 30;             % Frequency of density control checks.
n_sdim  = size(bbox,2);
if( n_sdim==2 )
  dp_tol   = -0.001*h0;   % Abs point rejection tol (p(dist(p)>=dp0_tol) are rejected).
  dtrm_tol = -0.001*h0;   % Abs dist tol for tri rejection (t(dist(p_tcent)>=dtrm_tol) are rejected).
  rt_tol   =  0.3;        % Rel fraction of h0 to trigger retriangulation.
  F_scale  =  1.2;        % Rel force scaling factor.
  F_DCF    =  2;          % Fraction of L to L_target to allow.
  dp_scale =  0.2;        % Rel fraction of computed new distance to move points in update step.
else
  dp_tol   = -0.1*h0;
  dtrm_tol = -0.1*h0;
  rt_tol   =  0.1;
  F_scale  =  1.1;
  F_DCF    =  2.1;
  dp_scale =  0.1;
end
dpc_tol = 0.001*h0;       % Abs tol for grid point movements during convergence check.
gradeps = sqrt(eps)*h0;   % Gradient computation offset.
%------------------------------------------------------------------------------%


% Initial grid point distribution, p, confined to the bounding box.
for i=1:n_sdim
  if( n_sdim==2 && i==2 )
    pinit{i} = bbox(1,i):h0*sqrt(3)/2:bbox(2,i);
  else
    pinit{i} = bbox(1,i):h0:bbox(2,i);
  end
end
pp = cell(1,n_sdim);
[pp{:}] = ndgrid( pinit{:} );
if( n_sdim==2 )
  pp{1}(:,2:2:end) = pp{1}(:,2:2:end) + h0/2;
end
p = zeros(prod(size(pp{1})),n_sdim);
for i=1:n_sdim
  p(:,i) = pp{i}(:);
end

% Remove points outside the region and apply the rejection method.
p = p( l_call_function(fd,p)<-dp_tol, : );
t = [];
stat = [];
if( isempty(p) )
  return
end


r0 = l_call_function(fh,p);   % Probability to keep point.
p  = p( rand(size(p,1),1) < min(r0)^n_sdim./r0.^n_sdim, : );
p_fix   = l_deduplicate( p_fix );
n_p_fix = size(p_fix,1);
if( ~isempty(p_fix) )
  p = [ p_fix; setdiff(p,p_fix,'rows') ];
end
n_p = size( p, 1 );


l_message( fid, 'Grid generation (DistMesh):' )
t1 = tic;
if( it_max<=0 )
  t = l_delaunay_triangulation( p, e_fix );
end
t_tri = toc(t1);
it = 0;
p0 = inf;
n_tri = 0;
n_dcs = 0;
do_break = false;
is_converged = false;
while( it<it_max )
    it = it + 1;

    % Retriangulate, if grid points have moved significantly.
    delta_p_max = max( sqrt(sum((p-p0).^2,2)) );
    if( rt_tol*h0<delta_p_max )
      n_tri = n_tri + 1;

      [p,t,td] = l_triangulate( p, fd, e_fix, dtrm_tol );
      if( iscell(e_fix) && it>IT_MINC )
        [p,t] = l_call_function( e_fix, p, t, n_sdim, 1:n_p_fix );
      end
      p0  = p;
      n_p = size(p,1);
      t_tri = t_tri + td;

      % clf, l_plot(p,t), title(['retriangulated mesh ',num2str(n_tri)]), drawnow, pause

      % Describe each edge by a unique edge_pairs of nodes.
      if( IALG<=1 )
        edge_pairs = zeros(0,2);
        local_edge_pairs = nchoosek(1:(n_sdim+1),2);
        for i=1:size(local_edge_pairs,1)
          edge_pairs = [edge_pairs;t(:,local_edge_pairs(i,:))];
        end
        edge_pairs = unique(sort(edge_pairs,2),'rows');
      else
        e = [ t(:,[1,2]); t(:,[2,3]); t(:,[3,1]) ];
        if( n_sdim==3 )
          e = [ e; t(:,[1,4]); t(:,[2,4]); t(:,[3,4]) ];
        end
        e = sort(e,2);
        e_max = max(e(:));
        if( e_max*(e_max+1)<realmax )
          ecomp = (e_max+1)*e(:,1) + e(:,2);
          [tmp,ind] = unique( ecomp );
          edge_pairs = e(ind,:);
        else
          edge_pairs = unique( e, 'rows' );
        end
      end

    end


    % Move mesh points based on edge lengths L and forces F.
    p1 = p(edge_pairs(:,1),:);
    p2 = p(edge_pairs(:,2),:);
    bars = p1 - p2;              % Bar vectors.
    L = sqrt(sum(bars.^2,2));    % Bar lengths.
    hbars = l_call_function( fh,0.5*( p1 + p2 ) );   % Rel bar mid point sizes.
    L_target = hbars*F_scale*(sum(L.^n_sdim)/sum(hbars.^n_sdim))^(1/n_sdim);   % Bar target lengths.


    % Density control, remove points that are too close to each other.
    if( mod(it,N_DCF)==0 && any(L_target>F_DCF*L) )
      n_dcs = n_dcs + 1;
      p(setdiff(reshape(edge_pairs(L_target>F_DCF*L,:),[],1),1:n_p_fix),:) = [];
      n_p = size(p,1);
      p0  = inf;
      continue;
    end

    % Compute grid point movements.
    F = max( L_target-L, 0 );   % Scalar bar forces.
    if( IALG<=1 )
      F_bar = [bars,-bars].*repmat(F./L,1,2*n_sdim);
      delta_p = full(sparse( edge_pairs(:,[ones(1,n_sdim), 2*ones(1,n_sdim)]), ...
                             ones(size(edge_pairs,1),1)*[1:n_sdim,1:n_sdim], ...
                             F_bar, n_p, n_sdim ));
    else
      F_bar = F./L*ones(1,n_sdim).*bars;
      delta_p = [];
      for i=1:n_sdim
        delta_p = [ delta_p, ...
                    accumarray(edge_pairs(:),[F_bar(:,i); -F_bar(:,i)],[n_p,1]) ];
      end
    end
    delta_p(1:n_p_fix,:) = 0;
    delta_p = dp_scale * delta_p;
    p = p + delta_p;


    % Move grid points outside geometry back to the boundary.
    for jt=1:N_RECV

      dist = l_call_function( fd, p );
      ix = dist > 0;
      ix(1:n_p_fix) = 0;
      if( any(ix) )
        grad_dist = zeros(sum(ix),n_sdim);
        for i=1:n_sdim
          doff = zeros(1,n_sdim);
          doff(i) = gradeps;
          dist_offset_i = l_call_function( fd, p(ix,:)+ones(sum(ix),1)*doff );
          grad_dist(:,i) = ( dist_offset_i - dist(ix) )/gradeps;
        end
        gradnm = sum( grad_dist.^2, 2 );
        p(ix,:) = p(ix,:) - (dist(ix)./gradnm*ones(1,n_sdim) .* grad_dist);
      end

    end


    % Statistics/output.
    delta_p_max = abs( max( [sqrt(sum(delta_p(dist<dp_tol,:).^2,2)); -inf] ) );
    if( ~mod(it,IT_PRT) )
      s = sprintf( 'Iteration %4i: %i vertices, %i cells, max(delta_p) = %g\n', it, size(p,1), size(t,1), delta_p_max );
      l_message( fid, s )
      if( ~isempty(fit) )
        do_break = l_call_function( fit, it );
      end
    end


    % Check for convergence.
    if( (it>IT_MIN && delta_p_max<dpc_tol) || size(t,1)<=2 || it>it_max || do_break )
      if( delta_p_max<dpc_tol )
        is_converged = true;
      end
      break;
    end

    % clf, l_plot(p,t), title(num2str(it)), drawnow, pause
end


% Clean up and check final mesh.
[p,t,td] = l_fixmesh( p, t, fd, e_fix, dtrm_tol );
t_tri = t_tri + td;


% Statistics.
t_tot = toc(t0);
if( nargout>=3 )
  stat.conv = is_converged;
  stat.nit  = it;
  stat.ntri = n_tri;
  stat.ndcs = n_dcs;
  stat.dpmx = max(sqrt(sum(delta_p(dist<-dp_tol,:).^2,2)));
  stat.dpmn = mean(sqrt(sum(delta_p(dist<-dp_tol,:).^2,2)));
  stat.ttot = t_tot;
  stat.ttri = t_tri;
end

if( do_break )
  s = 'stopped';
else
  s = 'done';
end
s = sprintf( 'Mesh generation %s: t_tot = %f s, %i iterations, %i (re-)triangulations\n', ...
             s, t_tot, it, n_tri );

l_message( fid, s )
% clf, l_plot(p,t), title('final mesh'), drawnow, pause


%------------------------------------------------------------------------------%
function [ p, t, td ] = l_triangulate( p, fd, e_fix, dtrm_tol )

if( nargin<3 )
  e_fix = [];
end

AV_TOL = eps*1e1;   % Minimum accepted absolute area/volume.

[is_nan,tmp] = find( isnan(p) );
p(is_nan,:)  = [];
p = l_deduplicate( p );


% Generate triangulation for grid points p.
t1 = tic;
t = l_delaunay_triangulation( p, e_fix );
td = toc(t1);


% Calculate simplex centers.
pc = [];
for i=1:size(p,2)
  pc = [ pc, mean(reshape(p(t,i),size(t)),2) ];
end

% Remove simplices with center outside region.
dist = l_call_function(fd,pc);
t = t(dist<dtrm_tol,:);

% Reorient simplices.
av = l_simpvol( p, t );
ix_flip = av<0;
t(ix_flip,[1,2]) = t(ix_flip,[2,1]);

% Remove simplices with volume < AV_TOL.
t(abs(av)<AV_TOL,:) = [];

if( isempty(t) )
  t = l_delaunay_triangulation( p, e_fix );
end

%------------------------------------------------------------------------------%
function [ t ] = l_delaunay_triangulation( p, c )

if( nargin<2 )
  c = [];
end

IS_WARN       = false;
USE_DELAUNAYN = true;
IS_CONSTR     = isnumeric(c) & ~isempty(c);

if( size(p,2)==3 && USE_DELAUNAYN )
  t = delaunayn( p );
else
  if( ~isempty(c) && IS_CONSTR && exist('DelaunayTriangulation') && size(p,2)==2 )
    try
      if( ~IS_WARN )
        warning('off','MATLAB:delaunayTriangulation:ConsSplitPtWarnId')
      end
      t = delaunayTriangulation( p, c );
      if( ~IS_WARN )
        warning('on','MATLAB:delaunayTriangulation:ConsSplitPtWarnId')
      end
    catch
      t = delaunay( p );
    end

  elseif( exist('DelaunayTri') )

    if( size(p,2)==3 )
      t = DelaunayTri( p(:,1), p(:,2), p(:,3) );
    else
      t = DelaunayTri( p(:,1), p(:,2) );
    end

  elseif( size(p,2)==3 && exist('delaunay3') && ...
          ~exist('OCTAVE_VERSION','builtin') )

    t = delaunay3( p(:,1), p(:,2), p(:,3) );

  else

    t = delaunay( p );

  end
end

if( isa(t,'DelaunayTri') )
  t = t.Triangulation;
end
if( isa(t,'delaunayTriangulation') )
  t = t.ConnectivityList;
end

%------------------------------------------------------------------------------%
function [ p, t, td, ind_p ] = l_fixmesh( p, t, fd, e_fix, dtrm_tol )
% FIXMESH Remove duplicated/unused nodes and fix element orientation.

if( nargin>=2 && (isempty(p) || isempty(t)) )
  ind_p = 1:size(p,1);
  return
end

P_TOL = eps*1024;
[p,ix,ind_p_orig] = l_deduplicate( p, P_TOL );


if( nargin>=2 )
  t = ind_p_orig(t);

  % Final triangulation.
  [p,t,td] = l_triangulate( p, fd, e_fix, dtrm_tol );

  % Calculate simplex centers.
  pc = [];
  for i=1:size(p,2)
    pc = [ pc, mean(reshape(p(t,i),size(t)),2) ];
  end

  % Remove simplices with center outside region.
  dist = l_call_function(fd,pc);
  t = t(dist<dtrm_tol,:);

  % Remove unused nodes.
  [ind_p,ix1,jx1] = unique( t );
  t = reshape( jx1, size(t) );
  p = p( ind_p, : );
  ind_p = ix( ind_p );

end

%------------------------------------------------------------------------------%
function [ v ] = l_simpvol( p, t );
% SIMPVOL Simplex volume.

switch( size(p,2) )
  case 1
    d12 = p(t(:,2),:) - p(t(:,1),:);
    v = d12;
  case 2
    d12 = p(t(:,2),:) - p(t(:,1),:);
    d13 = p(t(:,3),:) - p(t(:,1),:);
    v = ( d12(:,1).*d13(:,2) - d12(:,2).*d13(:,1) )/2;
  case 3
    d12 = p(t(:,2),:) - p(t(:,1),:);
    d13 = p(t(:,3),:) - p(t(:,1),:);
    d14 = p(t(:,4),:) - p(t(:,1),:);
    v = dot( cross(d12,d13,2), d14, 2 )/6;
  otherwise
    v = zeros(size(t,1),1);
    for ii=1:size(t,1)
      a = zeros(size(p,2)+1);
      a(:,1) = 1;
      for jj=1:size(p,2)+1
        a(jj,2:end)=p(t(ii,jj),:);
      end
      v(ii) = det(a);
    end
    v = v/factorial(size(p,2));
end

%------------------------------------------------------------------------------%
function [ b, i, j ] = l_deduplicate( a, atol )

if( isempty(a) )
  b = []; return;
end
if( nargin>=2 )
  s = atol;
else
  TOL = 1e-6;
  s = TOL*max(max(a)-min(a));
end
[c,k] = sortrows(s*round(a/s));
ix = any(c(1:size(c,1)-1,:)~=c(2:size(c,1),:),2);
j(k) = cumsum([1;ix]);
i = k([1;find(ix)+1]);
if( nargout>2 )
  [i,jj] = sort(i);
  kk(jj) = 1:numel(jj);
  j = kk(j).';
else
  i = sort(i);
end
b = a(i,:);

%------------------------------------------------------------------------------%
function [ varargout ] = l_call_function( fun, varargin )

if( isa(fun,'function_handle') )

  varargout = cell(1,max(1,nargout(fun)));
  [varargout{:}] = fun( varargin{:} );

elseif( iscell(fun) && (isa(fun{1},'function_handle') || ischar(fun{1})) )
  args = fun(2:end);
  fun  = fun{1};
  if( ischar(fun) )
    fun = str2func(fun);
  end

  empty_pos = find(cellfun(@isempty,args));
  if( ~isempty(empty_pos) )
    for i=1:length(varargin)
      if( i<=length(empty_pos) )
        args{empty_pos(i)} = varargin{i};
      else
        args = [ args, varargin{i} ];
      end
    end
  else
    args = [ varargin, args ];
  end

  varargout = cell(1,max(1,nargout(fun)));
  [varargout{:}] = fun( args{:} );

end

if( nargout>0 && ~iscell(varargout) )
  varargout = { varargout };
end

%------------------------------------------------------------------------------%
function l_message( fid, s )

if( isscalar(fid) && isnumeric(fid) && fid>0 )
  if( ~any(double(s(end))==[10,13]) )
    s = [s,char(10)];
  end
  fprintf( fid, s );
elseif( isa(fid,'function_handle') )
  fid( s );
end

%------------------------------------------------------------------------------%
function l_plot( p, t )

COLOR = [.9, .9, .9];
if( size(p,2)==2 )
  patch( 'vertices', p, 'faces', t, 'facecolor', COLOR )
else
  f = [t(:,[1:3]); t(:,[1,2,4]); t(:,[2,3,4]); t(:,[3,1,4])];
  patch( 'vertices', p, 'faces', f, 'facecolor', COLOR )
  view(3)
  rotate3d('on')
end
axis equal

%------------------------------------------------------------------------------%
function [ dist ] = l_dpolygon( p, v )

n_p = size(p,1);
n_s = size(v,1)-1;

dist = zeros(n_s,n_p);
for i_s=1:n_s
  v_i = v([i_s,i_s+1],:);
  n_p = size(p,1);
  w  = v_i(2,:)-v_i(1,:);
  ix1 = ones(n_p,1);
  vp = v_i(ix1,:)-p;
  w1 = w(ix1,:);
  s = dot(w1,vp,2);
  u = -s/(w*w.');
  u(u<0) = 0;
  u(u>1) = 1;
  h = w1.*[u,u]+vp;
  dist(i_s,:) = sqrt(dot(h,h,2));
end
dist = (-1).^(inpolygon(p(:,1),p(:,2),v(:,1),v(:,2))).*min(dist).';
