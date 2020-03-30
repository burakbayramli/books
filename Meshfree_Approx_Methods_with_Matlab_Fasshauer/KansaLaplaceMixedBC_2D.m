% KansaLaplaceMixedBC_2D
% Script that performs Kansa collocation for 2D Laplace equation
% with mixed BCs
% Calls on: CreatePoints, DistanceMatrix, DifferenceMatrix, PlotSurf,
%           PlotError2D 
  % IMQ RBF and its Laplacian
  rbf = @(e,r) 1./sqrt(1+(e*r).^2); ep = 3;
  dyrbf = @(e,r,dy) -dy*e^2./(1+(e*r).^2).^(3/2);
  Lrbf = @(e,r) e^2*((e*r).^2-2)./(1+(e*r).^2).^(5/2);
  % Exact solution and its Laplacian for test problem
  u = @(x,y) 1-0.9*x.^3+0*y;
  Lu = @(x,y) -5.4*x+0*y;
  % Collocation points and centers
  N = 289;  [collpts, N] = CreatePoints(N, 2, 'u');
  indx1 = find(collpts(:,2)==0 & collpts(:,1)~=1);  % Gamma_1
  indx2 = find(collpts(:,1)==1 & collpts(:,2)~=1);  % Gamma_2
  indx3 = find(collpts(:,2)==1 & collpts(:,1)~=0);  % Gamma_3
  indx4 = find(collpts(:,1)==0 & collpts(:,2)~=0);  % Gamma_4
  bdypts = collpts([indx1;indx2;indx3;indx4],:); % uniform boundary points
  [intpts, N] = CreatePoints(N, 2, 'c');  % interior Halton points
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
  dy_bdy = DifferenceMatrix(bdypts(:,2),ctrs(:,2));
  LCM = Lrbf(ep,DM_int);
  BCM1 = -dyrbf(ep,DM_bdy(1:sn-1,:),dy_bdy(1:sn-1,:));
  BCM2 = rbf(ep,DM_bdy(sn:2*sn-2,:));
  BCM3 = dyrbf(ep,DM_bdy(2*sn-1:3*sn-3,:),...
               dy_bdy(2*sn-1:3*sn-3,:));
  BCM4 = rbf(ep,DM_bdy(3*sn-2:end,:));
  CM = [LCM; BCM1; BCM2; BCM3; BCM4];
  % Create right-hand side
  rhs = [Lu(intpts(:,1),intpts(:,2)); zeros(sn-1,1); ...
         0.1*ones(sn-1,1); zeros(sn-1,1); ones(sn-1,1)];
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
  caption = ['Nonsymmetric RBF solution '...
      'false colored by maximum error.'];
  xe = reshape(epoints(:,1),sqrt(M),sqrt(M));
  ye = reshape(epoints(:,2),sqrt(M),sqrt(M));
  PlotSurf(xe,ye,Pf,sqrt(M),exact,maxerr,fview,caption);
  caption = 'Maximum error for nonsymmetric RBF solution.';
  PlotError2D(xe,ye,Pf,exact,maxerr,sqrt(M),fview,caption)
