% KansaLaplace_2D
% Script that performs Kansa collocation for 2D Laplace equation
% Calls on: CreatePoints, DistanceMatrix, PlotSurf, PlotError2D
  % IMQ RBF and its Laplacian
  rbf = @(e,r) 1./sqrt(1+(e*r).^2); ep = 3;
  Lrbf = @(e,r) e^2*((e*r).^2-2)./(1+(e*r).^2).^(5/2);
  % Exact solution and its Laplacian for test problem
  u = @(x,y) sin(pi*x).*cos(pi*y/2);
  Lu = @(x,y) -1.25*pi^2*sin(pi*x).*cos(pi*y/2);
  % Collocation points and centers
  N = 289;  [collpts, N] = CreatePoints(N, 2, 'u');
  indx = find(collpts(:,1)==0 | collpts(:,2)==0 | ...
              collpts(:,1)==1 | collpts(:,2)==1);
  bdypts = collpts(indx,:); %%% finding the points on the boundary 
  intpts = collpts(setdiff([1:N],indx),:); %% finds interior points
  ctrs = [intpts; bdypts];
  % Create neval-by-neval equally spaced evaluation locations
  % in the unit square
  M = 1600;  epoints = CreatePoints(M,2,'u');
  % Compute evaluation matrix
  DM_eval = DistanceMatrix(epoints,ctrs);
  EM = rbf(ep,DM_eval);
  exact = u(epoints(:,1),epoints(:,2));
  % Compute blocks for collocation matrix
  DM_int = DistanceMatrix(intpts,ctrs);
  LCM = Lrbf(ep,DM_int);
  DM_bdy = DistanceMatrix(bdypts,ctrs);
  BCM = rbf(ep,DM_bdy);
  CM = [LCM; BCM];
  % Create right-hand side
  rhs = [Lu(intpts(:,1),intpts(:,2)); ...
         u(bdypts(:,1),bdypts(:,2))];
  %rhs = zeros(N,1);  NI = size(intpts,1);
  %rhs(1:NI) = Lu(intpts(:,1),intpts(:,2));
  %indx = find(bdypts(:,2)==0);
  %rhs(NI+indx) = sin(pi*bdypts(indx,1));
  % Compute RBF solution
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
  plot(bdypts(:,1),bdypts(:,2),'go');  hold off
  fview = [-30,30];  % viewing angles for plot
  xe = reshape(epoints(:,1),sqrt(M),sqrt(M));
  ye = reshape(epoints(:,2),sqrt(M),sqrt(M));
  caption = ['Nonsymmetric RBF solution '...
      'false colored by maximum error.'];
  PlotSurf(xe,ye,Pf,sqrt(M),exact,maxerr,fview,caption);
  caption = 'Maximum error for nonsymmetric RBF solution.';
  PlotError2D(xe,ye,Pf,exact,maxerr,sqrt(M),fview,caption)
