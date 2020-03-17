% Generalized Principal Component Analysis, Chapter 2
% Examples 2.7, 2.10, 2.11
%
% Dependencies:
% - Download plottoolbox and put it in ./Toolboxes/plottoolbox
% - Download EYaleB dataset and put the subfolders in
% ./Databases/EYaleB_Cropped. The dataset is available at http://vision.ucsd.edu/extyaleb/CroppedYaleBZip/CroppedYale.zip
% - Download loadimage_EYaleB.m and put it into ./Databases/EYaleB_Cropped.

clc; clear all; close all;

addpath( fullfile('./Toolboxes', 'plottoolbox') ); % add toolboxes
addpath( fullfile('./Databases', 'EYaleB_Cropped') );% add face database
%% Read images
scale = 0.5;
imageSize = [192, 168] * scale; % size of the face images
D = imageSize(1) * imageSize(2); % dimension of the original data
conditionList = char(...
    'A+000E+00',...
    'A+020E-40',...
    'A+060E+20',...
    'A+085E+20',...
    'A+110E+40',...
    'A+000E+45',...
    'A-020E-40',...
    'A-035E+65',...
    'A-070E-35',...
    'A-110E+40'...
    );
X = loadimage_EYaleB('20', 'P00', conditionList, scale); % read frontal images
N = size(X, 2);
%% Example 2.7
fprintf('Example 2.7: PCA for modeling face images under varying illuminations\n')
fprintf('This code generates Figure 2.3 and Figure 2.4\n')
% Set d
d = 2; % dimension of projected data.
% Compute eigenfaces
[coeff,score,latent,tsquared,explained,mu] = pca(X', 'NumComponents', d);
% Plot mean and eigenfaces
figure;imshow( imnormalize(imresize(reshape(mu', imageSize), 1/scale)) ); title('Figure 2.3 (a)'); pause(1);
figure;imshow( imnormalize(imresize(reshape(coeff(:, 1), imageSize), 1/scale)) ); title('Figure 2.3 (b)'); pause(1);
figure;imshow( imnormalize(imresize(reshape(coeff(:, 2), imageSize), 1/scale)) ); title('Figure 2.3 (c)'); pause(1);
% Plot mean face plus varying magnitude of the first eigenface
figure; 
sigma1 = std(score(:, 1));
y1_vec = -1:1/3:1;
for iy1 = 1:length(y1_vec)
    y1 = y1_vec(iy1);
    imshow( imnormalize(imresize(reshape(mu' + sigma1 * y1 * coeff(:, 1), imageSize), 1/scale)) ); 
    title('Figure 2.4 (a)');
    pause(0.3);
end
% Plot mean face plus varying magnitude of the second eigenface
figure; 
sigma2 = std(score(:, 2));
y2_vec = -1:1/3:1;
for iy2 = 1:length(y2_vec)
    y2 = y2_vec(iy2);
    imshow( imnormalize(imresize(reshape(mu' + sigma2 * y2 * coeff(:, 2), imageSize), 1/scale)) );
    title('Figure 2.4 (b)'); 
    pause(0.3);
end
fprintf('Press any key to continue...\n\n');
pause;

%% Example 2.10
fprintf('Example 2.10: PPCA for modeling face images under varying illuminations\n')
fprintf('This code generates Figure 2.3 and Figure 2.4\n')

S = svds( bsxfun(@minus, X, mu') / sqrt(N), d ); 
sigma = sqrt( (trace( bsxfun(@minus, X, mu')'*bsxfun(@minus, X, mu') / N ) - sum(S .^2)) / (D - d) );
coeff = coeff * sqrt( diag(S).^2 - sigma ^2 * eye(d));
% Plot mean and eigenfaces
figure;imshow( imnormalize(imresize(reshape(mu', imageSize), 1/scale)) ); title('Figure 2.5 (a)'); pause(1);
figure;imshow( imnormalize(imresize(reshape(coeff(:, 1), imageSize), 1/scale)) ); title('Figure 2.5 (b)'); pause(1);
figure;imshow( imnormalize(imresize(reshape(coeff(:, 2), imageSize), 1/scale)) ); title('Figure 2.5 (c)'); pause(1);
% Plot mean face plus varying magnitude of the first eigenface\
figure; 
sigma1 = 1;
y1_vec = -1:1/3:1;
for iy1 = 1:length(y1_vec)
    y1 = y1_vec(iy1);
    imshow( imnormalize(imresize(reshape(mu' + sigma1 * y1 * coeff(:, 1), imageSize), 1/scale)) );
    title('Figure 2.6 (a)'); 
    pause(0.3);
end
% Plot mean face plus varying magnitude of the second eigenface
figure;
sigma2 = 1;
y2_vec = -1:1/3:1;
for iy2 = 1:length(y2_vec)
    y2 = y2_vec(iy2);
    imshow( imnormalize(imresize(reshape(mu' + sigma2 * y2 * coeff(:, 2), imageSize), 1/scale)) );
    title('Figure 2.6 (b)'); 
    pause(0.3);
end
fprintf('Press any key to continue...\n\n');
pause;
%% Model Selection
fprintf('Example 2.11: Model Selection for Face Images\n')
S_vals = svd(X)';
s_vals = sort( S_vals, 'descend' ); % Descending singular values.
s_squared_cum = fliplr( cumsum( fliplr(s_vals) .^2 ) ); % compute cumulative \sum_ {i=d+1} ^D sigma_i^2.
s_squared_ratio = s_squared_cum / s_squared_cum(1); % normalize above.

tau = 0.01:0.01:0.99; % parameter for model selection.
d_modelsel = zeros(1, length(tau));
for ii = 1 : length(tau)
    d_modelsel(ii) = find( s_squared_ratio < tau(ii), 1 );
end
plot(tau, d_modelsel);
xlabel('\tau'); ylabel('d');
axis([0 1 0 min(D, N)]); grid on;

