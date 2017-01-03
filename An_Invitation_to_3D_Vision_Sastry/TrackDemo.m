% FEATURE TRACKING DEMO
%
% Laboratory exercise in support of the textbook "An invitation to 3D vision", by
% Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS).
% Implements the basic feature tracking algorithm described in Chapter 11, Section 11.2
% DISTRIBUTED FREE FOR NON-COMMERCIAL USE
% Copyright (c) MASKS, 2003
%
% INPUT: filename for sequence of images and image type
% OUTPUT: 'good', index of good features, 'featx', x-coordinates of tracked features,
%         'featy', y-cordinates of tracked features (in pixels), 'featq', quality index for 
%         tracked features
% DESIGN/TUNING PARAMETERS: window sizes, thresholds, minimum distance, maximum number of features
%
% Contributors to this code include: Pietro Perona, Stefano Soatto, Andrea Mennucci, 
% Jean-Yves Bouguet, Xiaolin Feng, Hailin Jin, Paolo Favaro, Jana Kosecka, Yi Ma.
% Last updated 5/5/2003.

clear all;

% INPUT: insert filename, image numbers and type of image sequence here
% for instance, if your images are stored as image01.jpg, image02.jpg ... image99.jpg
% then seq_name = 'image', number list = 1:99, image_type = 'jpg', spaces = 2.

seq_name = 'sequence/A0000';
number_list = 2:2:20;
image_type = 'bmp';
spaces = 3;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interlaced = 0;     % Set to 1 if images are interlaced, e.g. when captured with analog camera
bPlot = 1;          % Set to 1 to get graphical output
generate_movie = 1; % set to 1 to generate a movie of tracked features. Requires bPlot to be set to 1.
FigureNumber = 1;   % output figure number
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global resolution winx winy saturation ...
    wintx winty spacing boundary boundary_t ...
    Nmax thresh levelmin levelmax ThreshQ ...
    N_max_feat method;

% TUNING PARAMETERS: parameters in this section of the code are free
% for the designer to choose. Choices are interconnected, and each choice affects
% the performance of the tracker.

resolution = 0.03; 			% Desired tracking accuracy in pixels
winx = 1; winy = 1;			% Window half-size for selection; effective size = (1+2*winx, 1+2*winy) 
				% THIS IS A CRUCIAL DESIGN PARAMETER
saturation = 7000; 			% Image saturation level (not necessary if variable 'method' chosen to be 0
wintx = 4; winty = 4; 			% Window half-size for tracking; effective size = (1+2*wintx, 1+2*winty)
spacing = 5;				% min spacing between 2 features (in pixel).
boundary = 5;				% discards features selected too close to the boundary of the image
boundary_t = 1;				% rejects tracked features too close to boundary
Nmax = 1000;                            % maximum number of features selected
thresh = 0.05; 				% Threshold for feature selection
				% THIS IS A CRUCIAL DESIGN PARAMETER: low threshold = many features selected,
					% greater errors; high threshold = fewer features selected, better quality. 
levelmin = 0; 				% lower level in the pyramid
levelmax = 2;				% higher level in the pyramid: large motions require more levels. 
					% Inter-frame motions within 1 pixel do not require a pyramid (levelmax=0).
ThreshQ = 0.1;				% Threshold for rejecting a feature
				% THIS IS A CRUCIAL DESIGN PARAMETER: low threshold = many features kept
					% through the track
N_max_feat = 500;			% Minimum space reserved for feature storage
method = 0;                             % Set to 1 to take into consideration
                                        % saturation effects (used in selection and tracking)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if interlaced,
  disp('Input images interlaced');
else
  disp('Input images non interlaced');
end;
if bPlot,
  disp('Graphical output on');
else
  disp('Graphical output off');
end;

if method,
  disp('Saturation on');
else
  disp('Saturation off');
end;

duration = length(number_list);
opt = sprintf('%%0%dd',spaces);

good = zeros(N_max_feat,duration);
featx = zeros(N_max_feat,duration);
featy = zeros(N_max_feat,duration);
featq = zeros(N_max_feat,duration);

%process the first image
index = sprintf(opt,number_list(1));
first = sprintf('%s%s.%s',seq_name,index,image_type);

tt2 = mean(double(imread(first)),3);
if interlaced,
  Ipi(:,:,1) = tt2(1:2:nx,:);
else,
  Ipi(:,:,1) = tt2;
end;
[nrow,ncol] = size(Ipi(:,:,1));

fprintf(1,'\n');
disp(['Feature selection on initial image ' first '...']);

%select feature points from Ipi
%only on the grid points
xtt = SelectFeature(Ipi);

Nini = size(xtt,2); 			% init # of features
if Nini < N_max_feat,
  xtt = [xtt,zeros(2,N_max_feat-Nini)];
  goodfeat = [ones(Nini,1);zeros(N_max_feat-Nini,1)];
else 
  xtt = xtt(:,1:N_max_feat);
  goodfeat = ones(N_max_feat,1);
end;

%%%%%%%%%%%%%%%%%%%%
%save the first image
Ifirst = Ipi(:,:,1);
xttfirst = xtt;
%%%%%%%%%%%%%%%%%%%%

fprintf(1, 'On initial image: %d features\n', size(find(goodfeat),1));

Qtt = ComputeQuality(Ipi,xtt,goodfeat,wintx,winty);% computes the quality vector Qtt from xtt and Ipi used to keep or reject tracked features.

good(:,1) = goodfeat;
featx(:,1) = xtt(1,:)';
featy(:,1) = xtt(2,:)';
featq(:,1) = Qtt;
								
if bPlot,
  % PLOT THE FIRST IMAGE !!!
  figure(FigureNumber);hold off; clf;
  image(Ipi(:,:,1));colormap(gray(256));
  axis('equal'); axis([1 ncol 1 nrow]);
  hold on;
  xf = xtt(:,find(goodfeat));
  if size(xf,1) > 0,
    plot(xf(2,:),xf(1,:), 'r+');
  end;
  hold off; drawnow;
  if generate_movie,
   M(1) = getframe;
  end
end;

tt2=Ipi(:,:,1);

%%% MAIN TRACKING LOOP 
for nbri=2:duration;

  %copy variable
  tt1=tt2;
  xt = xtt;
  
  seq = number_list(nbri);
  next = sprintf('%s%s.%s',seq_name,sprintf(opt,seq),image_type);
  fprintf(1, '\nTracking on image %s...\n',next);
  tt2 = mean(double(imread(next)),3);
  
  %track between image tt1 and tt2
  %also their downsampled versions
  %results are in xtt goodfeat Qtt
  if interlaced, tt2=tt2(1:2:nx,:); end;
  [xtt,goodfeat,Qtt] = track(tt1,tt2,xtt,goodfeat);

  %copy into lasting variables
  good(:,nbri) = goodfeat;
  featx(:,nbri) = xtt(1,:)';
  featy(:,nbri) = xtt(2,:)';
  featq(:,nbri) = Qtt;

  %%%%%%%%%%%%%%%%%  
  
  fprintf(1, 'After track #%d: %d features\n',nbri-1, size(find(goodfeat),1));
  
  %visualization
  if bPlot,
    %PLOT THE CURRENT IMAGE AND FEATURES
    figure(FigureNumber);
    image(tt2(:,:,1)); colormap(gray(256));
    axis('equal'); axis([1 ncol 1 nrow]);
    hold on;
    xf = xtt(:,find(goodfeat));
    if size(xf,1) > 0,
      plot(xf(2,:),xf(1,:), 'r+');
      ind_tracked = find(good(:,nbri-1) & good(:,nbri));
      if length(ind_tracked) > 0,
	plot([featy(ind_tracked,nbri-1),featy(ind_tracked,nbri)]',[featx(ind_tracked,nbri-1),featx(ind_tracked,nbri)]', 'y-');
      end;
    end;
    hold off; drawnow;
  if generate_movie,
   M(nbri) = getframe;
  end
  end;
end;
%%% MAIN TRACKING LOOP - ABOVE

if generate_movie, 
movie(M,-20,10);
end;

return;
save result good featx featy featq;
