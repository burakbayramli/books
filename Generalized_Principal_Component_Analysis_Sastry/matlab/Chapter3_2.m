% Generalized Principal Component Analysis, Chapter 3
% Examples 3.7, 3.12
%
% Dependencies:
% - Download plottoolbox and put it in ./Toolboxes/plottoolbox
% - Download RPCAtoolbox and put it in ./Toolboxes/RPCAtoolbox
% - Download exact_alm_rpca toolbox and put it in
% ./Toolboxed/exact_alm_rpca/. The toolbox is available at http://perception.csl.illinois.edu/matrix-rank/sample_code.html
% - Download inexact_alm_rpca toolbox and put it in
% ./Toolboxed/inexact_alm_rpca/. The toolbox is available at http://perception.csl.illinois.edu/matrix-rank/sample_code.html
% - Download EYaleB dataset and put the subfolders in
% ./Databases/EYaleB_Cropped. The dataset is available at http://vision.ucsd.edu/extyaleb/CroppedYaleBZip/CroppedYale.zip
% - Download loadimage_EYaleB.m and put it into ./Databases/EYaleB_Cropped.

clc;clear all;close all;

addpath( fullfile('./Toolboxes', 'RPCAtoolbox') ); % add toolboxes
addpath( fullfile('./Toolboxes', 'exact_alm_rpca') ); % add toolboxes
addpath( fullfile('./Toolboxes', 'inexact_alm_rpca') ); % add toolboxes
addpath( fullfile('./Toolboxes', 'plottoolbox') ); % add toolboxes
addpath( fullfile('./Databases', 'EYaleB_Cropped') );% add face database

%% Read images
scale = 0.5; % out-of-memory if scale = 1; 
             % try scale = 0.5 or scale = 0.25. The output images would be
             % upsampled to the original size (192*168).
imageSize = [192, 168] * scale; % size of the face images
D = imageSize(1) * imageSize(2); % dimension of the original data
X = loadimage_EYaleB('20', 'P00', [], scale); % read frontal images under all illuminations
N = size(X, 2);

face_IDs = [3 14 20 25]; % select from 1:N several faces to be plotted.

%% RPCA with corrupted entries
fprintf('Example 3.7, 3.12: Face Shadow Removal\n')
fprintf('This code generates Figure 3.4 and Figure 3.5\n')

% plot original images
for ii = 1:length(face_IDs)
    figure; imshow( imnormalize( imresize(reshape(X(:, face_IDs(ii)), imageSize), 1/scale) ) );
    title(['Figure 3.4(a)/3.5(a), column ' num2str(ii)]); pause(1);
end
RPCA_techniques = {
    'rls'; % reweighted least squares
    'exactalm'; % 
    'inexactalm'; % 
    };

% Robust PCA by reweighted least square
method = 'rls';
if ismember(method, RPCA_techniques)
    fprintf('RPCA by reweighted least square (rls)...\n');
    d = 4;
    epsilon0 = 1;
    [mu, Ud, Y] = rpca_rls(X, d, epsilon0);
    X_rls = bsxfun(@plus, mu, Ud*Y);
    
    for ii = 1:length(face_IDs)
        figure;
        subplot(1, 2, 1);imshow( imnormalize( imresize(reshape(X_rls(:, face_IDs(ii)), imageSize), 1/scale) ) );
        subplot(1, 2, 2);imshow( imnormalize( imresize(reshape(abs(X(:, face_IDs(ii)) - X_rls(:, face_IDs(ii))), imageSize), 1/scale) ) );
        suptitle(['Figure 3.4(b), column ' num2str(ii)]); pause(1);
    end
end
fprintf('Press any key to continue...\n\n');
pause;
% Robust PCA by convex optimization (by exact_alm)
method = 'exactalm';
if ismember(method, RPCA_techniques)
    fprintf('RPCA by convex optimization...\n');
    lambda = 1 / sqrt( max([D, N]) ); % weight on sparse error term in the cost function
    [A_hat, E_hat] = exact_alm_rpca(X, lambda);
    
    for ii = 1:length(face_IDs)
        figure;
        subplot(1, 2, 1);imshow( imnormalize( imresize(reshape(A_hat(:, face_IDs(ii)), imageSize), 1/scale) ) );
        subplot(1, 2, 2);imshow( imnormalize( imresize(reshape(abs(E_hat(:, face_IDs(ii))), imageSize), 1/scale) ) );
        suptitle(['Figure 3.5(b), column ' num2str(ii)]); pause(1);
    end
end
fprintf('Press any key to continue...\n\n');
pause;
% Robust PCA by convex optimization (by in_exact_alm)
method = 'inexactalm';
if ismember(method, RPCA_techniques)
    fprintf('RPCA by convex optimization ...\n');
    lambda = 1 / sqrt( max([D, N]) ); % weight on sparse error term in the cost function
    [A_hat, E_hat] = inexact_alm_rpca(X, lambda);
    for ii = 1:length(face_IDs)
        figure;
        subplot(1, 2, 1);imshow( imnormalize( imresize(reshape(A_hat(:, face_IDs(ii)), imageSize), 1/scale) ) );
        subplot(1, 2, 2);imshow( imnormalize( imresize(reshape(abs(E_hat(:, face_IDs(ii))), imageSize), 1/scale) ) );
        suptitle(['Figure 3.5(c), column ' num2str(ii)]); pause(1);
    end
end
fprintf('Press any key to continue...\n\n');
pause;