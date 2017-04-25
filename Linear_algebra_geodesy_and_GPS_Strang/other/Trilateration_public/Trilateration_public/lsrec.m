function [xn,Pn]=lsrec(varargin)
%LSREC Recursive Least Squares.
% [x,P] = LSREC(x0,W) initializes a recursive solution by returning the
% initial solution x = x0 having a scalar weight 0 < W <= 1. If x0 is a
% very good first estimate, use W near 1. If x0 is a poor first estimate
% use W near 0.  If W is not given, W = 1e-12 is used. P is a matrix of size
% length(x0)-by-length(x0) that is required for future recursive calls.
%
% [xn,Pn] = LSREC(yn,An,Wn,x,P) computes the recursive least squares
% solution xn, given new equations yn = An*x, where size(An,1) >= 1 and
% size(An,2) = length(x). Wn is the weight associated with the new data,
% which is typically equal to 1. If Wn is a scalar it applies to all new
% equations; if it is a vector the i-th element of Wn applies to the i-th
% equation. x and P are the output from the most recent recursive function
% call. xn and Pn are the updated solution vector and P matrix for future
% recursive calls.
%
% This function is useful when one wants to update a least squares solution
% repeatedly as new data becomes available, such as after each pass through
% some iterative process.
%
% Reference: "Modern Control Theory," 3rd ed., William L. Brogan
% Prentice Hall, 1991.
%
% See also MLDIVIDE, LSCOV, LSQNONNEG.

% D.C. Hanselman, University of Maine, Orono, ME 04469
% MasteringMatlab@yahoo.com
% Mastering MATLAB 7
% 2006-11-8

if nargin==1                                % initialize recursive solution
   xn=varargin{1}(:);
   Pn=diag(1e12+zeros(size(xn)));
   
elseif nargin==2                            % initialize recursive solution
   xn=varargin{1}(:);
   if numel(varargin{2})~=1
      error('LSREC:scalar','Scalar Weight Required.')
   else
      W=varargin{2};
      if W<=eps || W>1
         error('LSREC:OutofBound','Weight Must be Between 0 and 1.')
      end
      Pn=diag((1/W)+zeros(size(xn)));
   end
   
elseif nargin==5                                           % recursive call
   
   yn=varargin{1}(:); % make sure yn is a column vector
   An=varargin{2};
   Wn=varargin{3}(:);
   x=varargin{4}(:);
   P=varargin{5};
   if length(yn)~=size(An,1)
      error('LSREC:nonconform',...
            'yn Must Have as Many Rows as An.')
   end
   if size(An,2)~=length(x)
      error('LSREC:nonconform',...
            'An Must Have as Many Columns as x has elements.')
   end
   if size(P,1)~=size(P,2) || size(P,1)~=length(x)
      error('LSREC:nonform',...
            'P Must be a Square Matrix of Dimension Equal to length(x).')
   end
   if length(Wn)~=1 && length(Wn)~=length(yn)
      error('LSREC:conform',...
            'Wn Must be a Scalar or Have the Same Number of Elements as yn.')
   end
   if any(Wn<=eps) || any(Wn>1)
      error('LSREC:OutofBound','Weights Must be Between 0 and 1.')
   end
   if numel(Wn)==1   % expand scalar weight if needed
      Wn=repmat(Wn,size(yn));
   end
   
   K=P*An'/(An*P*An'+diag(1./Wn));
   xn=x+K*(yn-An*x);
   if nargout>1  % compute new P
      Pn=P-K*An*P;
   end
else
   error('LSREC:rhs','Recursive Calls Require 5 Input Arguments.')
end