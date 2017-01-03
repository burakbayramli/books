function [value,position] = findnear(A,b)
% PURPOSE: finds element in the input matrix (or vector) with 
%          value closest to the input value b 
% -----------------------------------------------------
% USAGE: [value,pos] = findnear(A,b)
%          where:  A = a matrix or vector
%                  b = value
% -----------------------------------------------------
% RETURNS: value = nearest value
%          pos   = scalar if A is a vector containing row element
%                = 1 x 2 vector is A is a matrix with row,col element
% -----------------------------------------------------

% I don't know who wrote this one

if nargin ~= 2
error('findnear: Wrong # of input arguments');
end;

   dist = abs(A-b);
          % The minimum of the distance vector
          tmp = repmat(min(dist(:)),size(dist));
   [pos(1),pos(2)] = find(dist == tmp);
   value = A(pos(1),pos(2));
          [n,m] = size(A);
          if m == 1
          position = pos(1);
          else
          position = pos;
          end;
  