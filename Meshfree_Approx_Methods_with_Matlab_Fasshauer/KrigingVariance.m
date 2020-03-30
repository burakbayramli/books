% Kriging Variance
% Computes and plots the kriging variance for 2D covariance kernels
% Also produces some plots of optimal kriging weights
% Calls on: DistanceMatrix
  K = @(e,r) exp(-(e*r).^2);  % Define the Gaussian RBF
  %K = @(epsilon,r) exp(-epsilon*r);    % basic Matern
  %K = @(epsilon,r) exp(-epsilon*r).*(3+3*epsilon*r+(epsilon*r).^2);   % Matern quadratic
  ep = 5; 
  N = 225; gridtype = 'u';
  dsites = CreatePoints(N,2,gridtype);
  ctrs = dsites;
  neval = 40; M = neval^2;
  epoints = CreatePoints(M,2,'u');
  DM_data = DistanceMatrix(dsites,ctrs);
  Kmatrix = K(ep,DM_data);
  DM_basis = DistanceMatrix(ctrs,epoints); % note that this is a matrix evaluating all
                                           % of the basis functions at epoints; one per row
  kvectors = K(ep,DM_basis);
  Kxx = K(ep,0);
  weights = Kmatrix\kvectors;
  krigingvariance = Kxx-sum(kvectors.*weights,1);
  % Plot kriging variance
  figure
  xe = reshape(epoints(:,1),neval,neval);
  ye = reshape(epoints(:,2),neval,neval);
  Kvplot = surf(xe,ye,reshape(krigingvariance,neval,neval));
  set(Kvplot,'FaceColor','interp','EdgeColor','none')
  set(gca,'ZScale','log')
  
  % Plot a kriging weight
  figure
  Kwplot = surf(xe,ye,reshape(weights(1,:),neval,neval));
  set(Kwplot,'FaceColor','interp','EdgeColor','none')
  colormap autumn; view([145 45]); camlight; lighting gouraud

  figure
  Kwplot = surf(xe,ye,reshape(weights(ceil(N/2),:),neval,neval));
  set(Kwplot,'FaceColor','interp','EdgeColor','none')
  colormap autumn; view([145 45]); camlight; lighting gouraud

  figure
  Kwplot = surf(xe,ye,reshape(weights(ceil(sqrt(N)/2),:),neval,neval));
  set(Kwplot,'FaceColor','interp','EdgeColor','none')
  colormap autumn; view([145 45]); camlight; lighting gouraud
