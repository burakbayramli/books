% Generalized Principal Component Analysis, Chapter 3
% Example 3.16
%
% Dependencies:
% - Download plottoolbox and put it in ./Toolboxes/plottoolbox
% - Download RPCAtoolbox and put it in ./Toolboxes/RPCAtoolbox
% - Download EYaleB dataset and put the subfolders in
% ./Databases/EYaleB_Cropped. The dataset is available at http://vision.ucsd.edu/extyaleb/CroppedYaleBZip/CroppedYale.zip
% - Download loadimage_EYaleB.m and put it into ./Databases/Outliers.
% - Prepare outlier images and put them in ./Databases/Images/ with naming
% "image_0001.jpg" to "image_0064.jpg". In example 3.16 for the book we
% used images from several categories from Caltech 101 dataset.
% - Download loadimage_outlier.m and put it into ./Databases/Outliers/

clc;clear all;close all;

addpath( fullfile('./Toolboxes', 'RPCAtoolbox') ); % add toolboxes
addpath( fullfile('./Toolboxes', 'plottoolbox') ); % add toolboxes
addpath( fullfile('./Databases', 'EYaleB_Cropped') );% add face database
addpath( fullfile('./Databases', 'Outliers') );% add face database

%% Read images
scale = 0.5; % out-of-memory if scale = 1; 
             % try scale = 0.5 or scale = 0.25. The output images would be
             % upsampled to the original size (192*168).
imageSize = [192, 168] * scale; % size of the face images
D = imageSize(1) * imageSize(2); % dimension of the original data
X_inlier = loadimage_EYaleB('20', 'P00', [], scale); % read frontal images under all illuminations
X_outlier = loadimage_outlier(scale);
N_inlier = size(X_inlier, 2);
N_outlier = size(X_outlier, 2);
N = N_inlier;
%% RPCA with outlier detection
fprintf('Example 3.16: Outlier Detection among Face Images\n')
fprintf('This code generates Figure 3.7\n')

outlier_perc_vec = [0.05 0.1 0.2 0.35 0.5 0.7];
for ii = 1:length(outlier_perc_vec)
    outlier_perc = outlier_perc_vec(ii);
    outlier_Size = round( outlier_perc * N );
    fprintf('\nWith %2.0f%% outliers:\n', outlier_perc * 100)
    X = [X_inlier(:, randperm(N_inlier, N - outlier_Size)), X_outlier(:, randperm(N_outlier, outlier_Size))];
    
    method = 'outlierPursuit';
    fprintf('RPCA by outlier pursuit...\n');
    tau = 5e-5;
    lambda0 = 3 / ( 7 * sqrt(outlier_perc * N) );
    lambda = 4 * lambda0;
    [L, E] = rpca(X, 'L21', tau, lambda);
    
    E_value = sum(E .^2, 1) .^.5;
    fig = figure;  hold on;
    plot( 1:N - outlier_Size, E_value(1:N - outlier_Size), '-b<' );
    plot( N - outlier_Size + 1 : N, E_value(N - outlier_Size + 1 : N), '-r*' )
    fig_legend = legend('Inliers', 'Outliers', 'Location', 'NorthWest');
%     set(fig_legend, 'FontSize', 18)
%     fig_axes = findall(fig, 'type', 'axes');
%     set(fig_axes, 'FontSize', 18)
    title(['Figure 3.7(' char(96+ii) ')']); pause(1);
end;
    