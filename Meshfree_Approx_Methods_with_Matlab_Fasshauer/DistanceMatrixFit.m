% DistanceMatrixFit
% Script that uses Euclidean distance matrices to perform
% scattered data interpolation for arbitrary space dimensions
% Calls on: DistanceMatrix, CreatePoints, testfunctiondD,
%           PlotSurf, PlotError2D, PlotSlices, PlotErrorSlices
% Uses:     various routines called by CreatePoints
  d = 2;
  k = 5; N = (2^k+1)^d;
  neval = 40; M = neval^d;
  % Use Halton points as data sites and centers
  dsites = CreatePoints(N,d,'h');
  ctrs = dsites;
  % Create neval^s equally spaced evaluation locations in the
  % s-dimensional unit cube (should be gridded for plots)
  epoints = CreatePoints(M,d,'u');
  % Create right-hand side vector,
  % i.e., evaluate the test function at the data sites
  rhs = testfunctiondD(dsites);
  % Compute distance matrix for the data sites and centers
  IM = DistanceMatrix(dsites,ctrs);
  % Compute distance matrix for evaluation points and centers
  EM = DistanceMatrix(epoints,ctrs);
  % Evaluate the interpolant on evaluation points
  % (evaluation matrix * solution of interpolation system)
  s = EM * (IM\rhs);
  % Compute exact solution,
  % i.e., evaluate test function on evaluation points
  exact = testfunctiondD(epoints);
  % Compute maximum and RMS errors on evaluation grid
  maxerr = norm(s-exact,inf);
  rms_err = norm(s-exact)/sqrt(M);
  fprintf('RMS error:     %e\n', rms_err)
  fprintf('Maximum error: %e\n', maxerr)
  switch d
     case 1
        plot(epoints, s)
        set(gca,'Fontsize',14)
        xlabel('x','FontSize',14);
        ylabel('y','FontSize',14,'Rotation',0);
        figure; plot(epoints, abs(s-exact))
        set(gca,'Fontsize',14)
        xlabel('x','FontSize',14);
        ylabel('Error','FontSize',14);
     case 2
        fview = [-30,30];
        xe = reshape(epoints(:,1),neval,neval);
        ye = reshape(epoints(:,2),neval,neval);
        caption = ['Distance matrix fit ',...
             'false colored by absolute error.'];
        PlotSurf(xe,ye,s,neval,exact,maxerr,fview,caption);
        caption = 'Maximum error for distance matrix fit.';
        PlotError2D(xe,ye,s,exact,maxerr,neval,fview,caption)
     case 3
         % seems backward, but this is what slice wants
        xe = reshape(epoints(:,2),neval,neval,neval);
        ye = reshape(epoints(:,3),neval,neval,neval);
        ze = reshape(epoints(:,1),neval,neval,neval);
        xslice = .25:.25:1; yslice = 1; zslice = [0,0.5];
        caption = 'Slice plot of distance matrix fit.';
        PlotSlices(xe,ye,ze,s,neval,xslice,yslice,zslice,caption);
        caption = ['Slice plot of absolute error ',...
            'for distance matrix fit.'];
        PlotErrorSlices(xe,ye,ze,s,exact,neval,...
            xslice,yslice,zslice,caption);
     otherwise
        disp('Cannot display plots for d>3')   
  end
