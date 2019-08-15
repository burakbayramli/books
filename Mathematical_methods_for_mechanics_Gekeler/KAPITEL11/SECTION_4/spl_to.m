function b=spl_to_bezier(s)
% spl_to_bezier: transforms a spline into bezier form
%  b = spl_to_bezier(s)
%
%  Input: s: structure with spline data in fields:
%            s.dimension: number d of coordinates
%            s.parameter: number p of parameters
%            s.degree:    vector of degrees 
%                         degree(i) is the degree for the i-th parameter
%            s.knots:     cell vector of length p with knots 
%                         knots{i} are the knots for the i-th parameter
%                         if p==1 knots may be a vector 
%            s.points:    p- or (p+1)-dimensional array with control-points
%                         the i-th dimension refers to the i-th parameter
%                         the p+1-st dimension refers to the coordinates  
%                         if size(points,p+1)==d+1 the points are homogenous
%                                    and points(:,..,:,d+1) are the weights
%                         if size(points,p+1)==d the points are cartesian
%
% Output: b: array of bezier-spline structures. 
%            fields like input without knots

% Author: Joerg Hoerner
% Last modification: Aug 1st, 2000 

%-----------------------------------------------------------------------------
% main program
%-----------------------------------------------------------------------------

b=[]; % initialize for early return

% test for input
if nargin <1
  fprintf(2,'spl_to_bezier: no spline!\n');
  return;
end;

% test if already bezier-Form
if ~(isfield(s,'knots'))
  fprintf(1,'spl_to_bezier: spline has bezier form. Nothing to do.\n');
  b=s;
  return;
end;

% test for needed fields
if ~(isfield(s,'points') &  isfield(s,'degree') & isfield(s,'parameter'))
  fprintf(2,'spl_to_bezier: error in input structure!\n');
  return;
end;

for k1=1:s.parameter
  % switch for univariate case      
  if iscell(s.knots)
    knots_to_insert{k1}=s.knots{k1}(:)*	ones(1,s.degree(k1)-1);
  else 
    knots_to_insert=s.knots(:)*ones(1,s.degree(k1));
  end
end;
s=spl_knot(s,knots_to_insert);

% calculate patch numbers
for k1=1:s.parameter
  % switch for univariate case      
  if iscell(s.knots)
    p_num(k1)=floor((length(s.knots{k1})-s.degree(k1)-2)/s.degree(k1));
  else 
    p_num=floor((length(s.knots)-s.degree-2)/s.degree);
  end
end;

% split the controll-point array
for k1=1:prod(p_num) % number of patches
  for k2=1:s.parameter % compute multi-index and point indices
    ind(k2)=mod(floor((k1-1)/prod(p_num(1:k2-1))),p_num(k2))+1;
    pind(:,k2)=[1+(ind(k2)-1)*s.degree(k2);1+ind(k2)*s.degree(1)];
  end;
  
  % we don't know how many parameter so we have to construct the command
  inds=sprintf('%d,',ind);
  pinds=sprintf('%d:%d,',pind);
  command=sprintf('b(%s).points=s.points(%s:);',inds(1:end-1),pinds);
  eval(command);  
end;

% degree, parameter and dimension is for every bezier-patch the same
[b.degree]=deal(s.degree);
[b.parameter]=deal(s.parameter);
[b.dimension]=deal(s.dimension);

















