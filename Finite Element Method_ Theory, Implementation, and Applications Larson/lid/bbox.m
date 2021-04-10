function [Box, Traversal] = bbox(X, Y, varargin)
% BBOX Calculates the 2D bounding box for the given points
%  
%   Box = BBOX(X, Y) calculates the bounding box for the given points in X and Y
%   position and returns a matrix of size 3x8 where each column is one of the
%   bounding box' corners.
%   Basically, what BBOX does is take all the mins and max' from the values of X
%   and Y and assigns them properly into box.
%
%   [Box, Traversal] = BBOX(X, Y) also returns the array of traversals which
%   relates to the corners of BOX to get a full patch
%
%   
%   Inputs:
%   
%   X: Vector or matrix of points on the X-axis
%   
%   Y: Vector or matrix of points on the Y-axis
%
%   Outputs:
%
%   BOX: Matrix of 3x8 entries that correspond the corners of the bounding box
%   with their relation as given in the second output parameter TRAVERSAL
%
%   TRAVERSAL: Matrix of 6 rows of size 3 where each row corresponds to one
%   traversal of the bounding box BOX for using the patch command
%



%% File information
% Author: Philipp Tempel <philipp.tempel@isw.uni-stuttgart.de>
% Date: 2017-08-04
% Changelog:
%   2017-08-04
%       * Convert all ```assert``` into ```validateattributes``` for better
%       error display
%   2016-05-30
%       * Update matrix argument processing to column major i.e., [X, Y]
%   2016-05-10
%       * Add END OF CODE block
%       * Rename to bbox
%       * Add possibility to pass an Nx2 matrix instead of two separate vectors
%   2016-04-13
%       * Fix bug on getting the min and max vals of X, Y
%   2016-04-01
%       * Initial release



%% Pre-process input
% If there's only one argument and it is a matrix ...
if ismatrix(X) && size(X, 2) == 2
    % ... grab Y from X
    Y = X(:,2);
    % ... and grab X from X
    X = X(:,1);
end



%% Validate inputs
try
    % X must not be a scalar and must be a vector
    validateattributes(X, {'numeric'}, {'vector', 'nonempty', 'finite', 'nonsparse', 'numel', numel(Y)}, mfilename, 'X');
    % Y must not be a scalar and must be a vector
    validateattributes(Y, {'numeric'}, {'vector', 'nonempty', 'finite', 'nonsparse', 'numel', numel(X)}, mfilename, 'Y');
catch me
    throwAsCaller(me);
end



%% Do the magic!
% First, get all minimum and maximum values of X, and Y
minVals = [min(X), min(Y)];
maxVals = [max(X), max(Y)];

% Holds our output
aBoundingBox = zeros(4, 2);

% The first set of points will be all points on the lower side of the cube
aBoundingBox(1, :) = [minVals(1), minVals(2)];
aBoundingBox(2, :) = [maxVals(1), minVals(2)];
aBoundingBox(3, :) = [maxVals(1), maxVals(2)];
aBoundingBox(4, :) = [minVals(1), maxVals(2)];

% This allows to use results of bbox as input to patch('Vertices', box, 'Faces', traversal)
aTraversal = [1, 2, 3, 4];



%% Assign output quantities
% Main output is the bounding box
Box = aBoundingBox;

% Second output is the traversal usable for ```patch``` command
if nargout > 1
    Traversal = aTraversal;
end


end

