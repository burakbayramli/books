% given a set of points generate lines between them
% in the structure it returns incidence relationships
function [l, inc] = houseLines_mixed(X)
npoints = size(X,2);

l(:,1) = [X(:,1); X(:,2)];  % 1-2
l(:,2) = [X(:,2); X(:,3)];  % 2-3
l(:,3) = [X(:,3); X(:,4)];  % 3-4
l(:,4) = [X(:,4); X(:,1)];  % 4-1
l(:,5) = [X(:,5); X(:,6)];  % 5-6
l(:,6) = [X(:,6); X(:,7)];  % 6-7
l(:,7) = [X(:,7); X(:,8)];  % 7-8
l(:,8) = [X(:,8); X(:,5)];  % 8-5
l(:,9) = [X(:,1); X(:,5)];  % 1-5
l(:,10) = [X(:,2); X(:,6)];  % 2-6
l(:,11) = [X(:,3); X(:,7)];  % 3-7
l(:,12) = [X(:,4); X(:,8)];  % 4-8
l(:,13) = [X(:,4); X(:,9)];  % 4-9
l(:,14) = [X(:,3); X(:,10)];  % 3-10
l(:,15) = [X(:,8); X(:,11)];  % 8-11
l(:,16) = [X(:,7); X(:,12)];  % 7-12
l(:,17) = [X(:,11); X(:,12)];  % 11-12
%l(:,17) = [X(:,1); X(:,12)];  % 11-12
l(:,18) = [X(:,12); X(:,10)];  % 12-10
l(:,19) = [X(:,9); X(:,10)];  % 9-10
l(:,20) = [X(:,9); X(:,11)];  % 9-11
%l(:,20) = [X(:,9); X(:,2)];  % 9-11

% put down indexes of lines incident to a point
inc(1).lines = [];
inc(1).lines = [1,4,9];
inc(2).lines = [1,2,10];
inc(3).lines = [2,3,11];
inc(4).lines = [3,4,12,13];
inc(5).lines = [5,8,9];
inc(6).lines = [6,10];
inc(7).lines = [6,7,11,16];
inc(8).lines = [7,8,12,15];
inc(9).lines = [13,19,20];
inc(10).lines = [14,18,19];
inc(11).lines = [15];
inc(12).lines = [16,17,18];
