% KansaEllipticVC_2D
% Script that performs Kansa collocation for 2D elliptic PDE
% with variable coefficients
% Calls on: CreatePoints, DistanceMatrix, DifferenceMatrix, PlotSurf,
%           PlotError2D 
  % IMQ RBF and its derivatives
  rbf = @(e,r) 1./sqrt(1+(e*r).^2); ep = 3;
  dxrbf = @(e,r,dx) -dx*e^2./(1+(e*r).^2).^(3/2);
  dyrbf = @(e,r,dy) -dy*e^2./(1+(e*r).^2).^(3/2);
  dxxrbf = @(e,r,dx) e^2*(3*(e*dx).^2-1-(e*r).^2)./...
                         (1+(e*r).^2).^(5/2);
  dyyrbf = @(e,r,dy) e^2*(3*(e*dy).^2-1-(e*r).^2)./...
                         (1+(e*r).^2).^(5/2);
  % Test problem input (right-hand side, coefficients)
  u = @(x,y) 16*x.*(1-x).*y.*(1-y);
  Lu = @(x,y) -16*x.*exp(x-y).*(1-x).*(3-2*y)+...
             32*y.*(1-y).*(3*x.^2+y.^2-x-2);
  a = @(x,y) 2-x.^2-y.^2; ax = @(x,y) -2*x;
  b = @(x,y) exp(x-y); by = @(x,y)-exp(x-y);
  % Collocation points and centers
  N = 289;  [collpts, N] = CreatePoints(N, 2, 'u');
  indx = find(collpts(:,1)==0 | collpts(:,2)==0 | ...
              collpts(:,1)==1 | collpts(:,2)==1);
  bdypts = collpts(indx,:); %%% find uniform points on the boundary 
  [intpts, N] = CreatePoints(N, 2, 'h');  % interior Halton points
  sn = sqrt(N);  h = 1/(sn-1);
  bdyctrs = bdypts;  bdyctrs = (1+2*h)*bdyctrs-h;
  ctrs = [intpts; bdyctrs];
  % Create neval-by-neval equally spaced evaluation locations
  % in the unit square
  M = 1600;  epoints = CreatePoints(M,2,'u');
  % Compute evaluation matrix
  DM_eval = DistanceMatrix(epoints,ctrs);
  EM = rbf(ep,DM_eval);
  exact = u(epoints(:,1),epoints(:,2));
  % Compute blocks for collocation matrix
  DM_int = DistanceMatrix(intpts,ctrs);
  DM_bdy = DistanceMatrix(bdypts,ctrs);
  dx_int = DifferenceMatrix(intpts(:,1),ctrs(:,1));
  dy_int = DifferenceMatrix(intpts(:,2),ctrs(:,2));
  LCM = diag(ax(intpts(:,1))) * dxrbf(ep,DM_int,dx_int) + ...
        diag(a(intpts(:,1),intpts(:,2))) * dxxrbf(ep,DM_int,dx_int) + ...
        diag(by(intpts(:,1),intpts(:,2))) * dyrbf(ep,DM_int,dy_int) + ...
        diag(b(intpts(:,1),intpts(:,2))) * dyyrbf(ep,DM_int,dy_int);
  BCM = rbf(ep,DM_bdy);
  CM = [LCM; BCM];
  % Create right-hand side
  rhs = [Lu(intpts(:,1),intpts(:,2)); zeros(4*(sn-1),1)];
  % RBF solution
  Pf = EM * (CM\rhs);
  % Compute maximum error on evaluation grid
  maxerr = norm(Pf-exact,inf);
  rms_err = norm(Pf-exact)/sqrt(M);
  fprintf('RMS error:     %e\n', rms_err)
  fprintf('Maximum error: %e\n', maxerr)
  % Plot collocation points and centers
  figure
  hold on; plot(intpts(:,1),intpts(:,2),'bo');
  plot(bdypts(:,1),bdypts(:,2),'rx');
  plot(bdyctrs(:,1),bdyctrs(:,2),'gx'); hold off
  fview = [-30,30];  % viewing angles for plot
  xe = reshape(epoints(:,1),sqrt(M),sqrt(M));
  ye = reshape(epoints(:,2),sqrt(M),sqrt(M));
  caption = ['Nonsymmetric RBF solution '...
      'false colored by maximum error.'];
  PlotSurf(xe,ye,Pf,sqrt(M),exact,maxerr,fview,caption);
  caption = 'Maximum error for nonsymmetric RBF solution.';
  PlotError2D(xe,ye,Pf,exact,maxerr,sqrt(M),fview,caption)
