function I = findmaxcol(X)
% I = findmaxcol(X)   Find the maximum column in each row
%     I is a column vector returning the column index of the maximum 
%     value in each row of vector X.  If X has several columns equal 
%     to the maximum in that row, the index of the first only is 
%     returned.
% 2001-03-28 dpwe@ee.columbia.edu 

[nrows,ncols] = size(X);

% Max of each row
maxx = max(X');

maxes = (X == (maxx'*ones(1,ncols))) .* (ones(nrows,1)*[ncols:-1:1]);

I = ncols+1-max(maxes');
