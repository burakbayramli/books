% Generalized Principal Component Analysis, Chapter 3
% Examples 3.3, 3.6
%
% Dependencies:
% - Download plottoolbox and put it in ./Toolboxes/plottoolbox
% - Download RPCAtoolbox and put it in ./Toolboxes/RPCAtoolbox
% - Download EYaleB dataset and put the subfolders in
% ./Databases/EYaleB_Cropped. The dataset is available at http://vision.ucsd.edu/extyaleb/CroppedYaleBZip/CroppedYale.zip
% - Download loadimage_EYaleB.m and put it into ./Databases/EYaleB_Cropped.

clc; clear all; close all;

addpath( fullfile('./Toolboxes', 'RPCAtoolbox') ); % add toolboxes
addpath( fullfile('./Toolboxes', 'plottoolbox') ); % add toolboxes
addpath( fullfile('./Databases', 'EYaleB_Cropped') );% add face database

%% Read images
scale = 0.50; % out-of-memory for scale = 1 for some methods that need matrix of size D by D; 
             % try scale = 0.5 or scale = 0.25. The final output images would be
             % upsampled to the original size.
imageSize = [192, 168] * scale; % size of the face images
D = imageSize(1) * imageSize(2); % dimension of the original data
X = loadimage_EYaleB('20', 'P00', [], scale); % read frontal images under all illuminations
N = size(X, 2);
%% Experiments
fprintf('Example 3.3, 3.6: Completing Face Images with Missing Pixels\n')
fprintf('This code generates Figure 3.2 and Figure 3.3\n')
MC_techniques = {
    'PF'; % power factorization.
    'svt2'; % singular value thresholding.
    };
missing_perc = [0.3 0.5 0.7 0.8 0.9];
for ii = 1:length(missing_perc)
    missrate = missing_perc(ii);
    fprintf('\nWith %2.0f%% missing percentage:\n', missrate * 100)
    % Generate missing entries
    Omega = ones(size(X));
    missing = randperm(D*N,round(missrate*D*N));
    Omega(missing) = 0;
    Xi = X .* Omega;
    % plot corrupted image (the first face)
    figure; imshow( imnormalize( imresize(reshape(Xi(:, 1), imageSize), 1/scale) ) ); title(['Figure 3.2/3.3, row 1, column ' num2str(ii)]); pause(1);
    
 %% Matrix Completion   
    method = 'PF';
    if ismember(method, MC_techniques)
        fprintf('Matrix completion by Power Factorization...\n');
        d_list = [2 4 6 9];
        for id = 1:length(d_list)
            d = d_list(id);
            [mu, Ud, Y, flag] = pf(Xi, d, Omega);
            % compute recovered matrix Xe
            if flag == 0
                Xe = bsxfun(@plus, mu, Ud * Y); 
            else
                Xe = Xi;
            end
            figure; imshow( imnormalize( imresize(reshape(Xe(:, 1), imageSize), 1/scale) ) );
            title(['Figure 3.3, row ' num2str(id+1) ', column ' num2str(ii)]); pause(1);
        end
    end
    method = 'svt2';
    if ismember(method, MC_techniques)
        fprintf('Matrix completion by svt...\n');
        tau_list = [1e3 2e4 4e5 8e6];
        for it = 1:length(tau_list)
            tau = tau_list(it); %5*sqrt(prod(size(X)));
            delta = min(2, prod(size(X))/sum(sum(Omega)));% see equation 5.1 in http://arxiv.org/pdf/0810.3286.pdf 

            [Xe,cost] = svt(Xi,Omega,tau,delta);

            figure; imshow( imnormalize( imresize(reshape(Xe(:, 1), imageSize), 1/scale) ) );
            title(['Figure 3.2, row ' num2str(id+1) ', column ' num2str(ii)]); pause(1);
        end
    end
end

