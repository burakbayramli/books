function cfdPlotVectors(X, Y, Z, U, V, W, S)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     This function plots cfdVector field over an existing figure
%--------------------------------------------------------------------------

q = quiver3(X, Y, Z, U, V, W, S);

% Compute the magnitude of the vectors
cfdMags = sqrt(sum(cat(2, q.UData(:), q.VData(:), ...
            reshape(q.WData, numel(q.UData), [])).^2, 2));

% Get the current colormap
currentColormap = colormap(gca);

% Determine the color to make each arrow using a colormap
[~, ~, ind] = histcounts(cfdMags, size(currentColormap, 1));

% Now map this to a colormap to get RGB
cmap = uint8(ind2rgb(ind(:), currentColormap) * 255);
cmap(:,:,4) = 255;
cmap = permute(repmat(cmap, [1 3 1]), [2 1 3]);

% Repeat each color 3 times (using 1:3 below) because each arrow has 3 vertices
set(q.Head, ...
    'ColorBinding', 'interpolated', ...
    'ColorData', reshape(cmap(1:3,:,:), [], 4).');   %'

% Repeat each color 2 times (using 1:2 below) because each tail has 2 vertices
set(q.Tail, ...
    'ColorBinding', 'interpolated', ...
    'ColorData', reshape(cmap(1:2,:,:), [], 4).');