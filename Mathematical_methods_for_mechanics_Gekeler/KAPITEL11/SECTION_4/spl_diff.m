function sd = spl_diff(s,o)
% spl_diff: differentiation of splines 
%  sd = spl_diff(s,o)
%
%  Input: s: structure with spline data in fields:
%            s.dimension: number d of coordinates
%            s.parameter: number p of parameters
%            s.degree:    vector of degrees 
%                         degree(i) is the degree for the i-th parameter
%            s.knots:     cell vector of length p with knots 
%                         knots{i} are the knots for the i-th parameter
%                         if p==1 knots may be a vector 
%                         if there are no knots, bezier-form is used
%            s.points:    p- or (p+1)-dimensional array with control-points
%                         the i-th dimension refers to the i-th parameter
%                         the p+1-st dimension refers to the coordinates  
%                         the points are cartesian
%                         rational splines are not supported
%         o: vector with orders of diffentiation for each parameter
%
%
% Output: sd: spline-structure, data as input

% Author: Joerg Hoerner
% Last modification: Sep 21st, 2001 

%-----------------------------------------------------------------------------
% main program
%-----------------------------------------------------------------------------

% number of coordinates
cdim=size(s.points,s.parameter+1);

% less coordinates than told in dimension field --> error
if cdim < s.dimension
  fprintf(2,'spl_diff: Error in Spline\n');
  sd=[];
  return;
end;

if size(s.points,s.parameter+1) > s.dimension
  fprintf(2,'spl_diff: Spline has to be non-rational\n');
  sd=[];
  return;
end;

if cdim == 1 & s.parameter == 1 % only one coordinate and one paramneter -->
  p=s.points(:);                % p has to be a column vector
else
  p=s.points;
end;

for k1=1:s.parameter % every parameter
  n=o(k1);
  
  bezier = ~isfield(s,'knots'); % if there are no knots --> bezier-form
  
  if ~bezier
    % spline switch for univariate case
    if iscell(s.knots)
      knots=s.knots{k1};
    else 
      knots=s.knots;
    end
  end;
    
  nums=size(p);
  p=reshape(p,nums(1),prod(nums(2:end))); % every column is a function 
  for k2=1:n         % order of differentiation    
    if bezier
      p=spl_diff_bez_curve(p);
    else    
      [p,knots]=spl_diff_spl_curve(p,knots);
    end;
  end;
  if ~bezier
    sd.knots{k1}=knots;
  end;
  [n1,n2]=size(p);                        
  p=reshape(p,[n1,nums(2:end)]);  % reorganize the parameter directions
  p=shiftdim(p,1);                % next direction to first place 
end;  

if cdim > 1 | s.parameter ==1 
  % more than one coordinate or only one parameter --> we need another shift
  p=shiftdim(p,1);
end;

sd.dimension=s.dimension;
sd.parameter=s.parameter;
sd.degree=max(zeros(1,sd.parameter),s.degree-o);
sd.points=p;


%-----------------------------------------------------------------------------
% subroutine spl_diff_spl_curve
%-----------------------------------------------------------------------------
function [pd,ud]=spl_diff_spl_curve(p,u)
% diffentiates spline curve with controllpoints p and knots u 
[p_num,f_num] = size(p);
degree = length(u) - p_num - 1;

if degree==0
  pd=zeros(size(p));
  ud=u;
  return;
end;


ind=find(u(2:p_num) < u(2+degree:p_num+degree))+1;
pd=degree.*(p(ind,:) - p(ind-1,:))./...
   ((u(ind+degree) - u(ind))'*ones(1,f_num));
ud=u([ind,[ind(end)+1:ind(end)+degree]]);

%-----------------------------------------------------------------------------
% subroutine spl_diff_bez_curve
%-----------------------------------------------------------------------------
function pd=spl_diff_bez_curve(p)
% evaluates bezier curve with controllpoints p at values t
[p_num,f_num] = size(p);
degree=p_num-1;

if degree==0
  pd=zeros(size(p));
  return;
end;

ind=2:pnum;
pd=degree*(p(ind,:) - p(ind-1,:));













