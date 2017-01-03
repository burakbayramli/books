function M=blockdiag(varargin)
% PURPOSE: Construct a block-diagonal matrix with the inputs on the diagonals.
% e.g. blockdiag(a,b,c) generates [a 0 0;0 b 0;0 0 c] where the zero-matrices are
%      of appropiate size. 
%------------------------------------------------------------------------
% USAGE: M = blockdiag(varargin)
%
% The function accepts arbitrary number of arguments, where
% each matrix can have an arbitrary size (and even arbitrary number
% of dimensions).
%------------------------------------------------------------------------
% RETURNS: M = [a 0 0;0 b 0;0 0 c] where the zero-matrices are
%                                  of appropiate size. 
%------------------------------------------------------------------------

% Written by:Hans.Olsson@dna.lth.se 1997-10-10
%            Department of Computer Science
%            Lund University in Sweden.

% HOW MANY DIMENSIONS?
% ----- Start of code for arbitrary number of dimensions.
ndtab=zeros(nargin,1);
for i=1:nargin,
  ndtab(i)=ndims(varargin{i});
end;

% Maximum number of dimensions.
ndmax=max(ndtab);
% ----- End of arbitrary number of dimensions code.
% Replace with 'ndmax=2;' for a few extra percent of speed increase.

% FIND SIZES OF INPUTS.
si=ones(nargin,ndmax); 
% Using ones guarantees that trailing matrix dimension have size 1.
for i=1:nargin
  si(i,1:ndtab(i))=size(varargin{i});
end;
  
% FILL IN ZEROS IN OUTPUT.
M=zeros(sum(si,1)); 
% Note the 1. Plain 'sum(si)' would fail if only one input to blockdiag.
  
% FILL IN BLOCKDIAGONALS.
start=zeros(1,ndmax);
if (ndmax==2)
  % Special case for 2-dimensional matrices.
  for i=1:nargin
    M(start(1)+(1:si(i,1)),start(2)+(1:si(i,2)))=varargin{i};
    start=start+si(i,:);
  end;
else
  % ----- Start of code for arbitrary number of dimensions.
  % The general case of n-dimensional matrices.
  % Basically only included for the fun of it.
  index={};
  for i=1:nargin
    for j=1:ndmax
      % index for dimension j:
      index{j}=start(j)+(1:si(i,j));
    end;
    % This transform the index-list into a list of arguments to M():
    % Compare to the 2-dimensional case.
    M(index{:})=varargin{i};
    start=start+si(i,:);
  end;
  % ----- End of code for arbitrary number of dimensions.
end;
