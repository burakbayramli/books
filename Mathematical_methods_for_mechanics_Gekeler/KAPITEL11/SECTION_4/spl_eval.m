function e = spl_eval(s,t)
% spl_eval: evaluation of splines 
%  e = spl_eval(s,t)
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
%                         if size(points,p+1)==d+1 the points are homogenous
%                                    and points(:,..,:,d+1) are the weights
%                         if size(points,p+1)==d the points are cartesian
%         t: cell vector with parameter values for evaluation
%            t{i} is a vector with the values for the i-th parameter
%            if p==1 t may be a vector
%
% Output: e: p+1 dimensional array with evaluation values
%            e is structured like s.points

% Author: Joerg Hoerner
% Last modification: Sep 21st, 2001 

%-----------------------------------------------------------------------------
% main program
%-----------------------------------------------------------------------------

% number of coordinates
cdim=size(s.points,s.parameter+1);

% less coordinates than told in dimension field --> error
if cdim < s.dimension | cdim > s.dimension+1
  fprintf(2,'spl_eval: Error in Spline\n');
  e=[];
  return;
end;

if cdim == 1 & s.parameter == 1 % only one coordinate and one paramneter -->
  e=s.points(:);                % e has to be a column vector
else
  e=s.points;
end;

for k1=1:s.parameter % evaluate every parameter

  % switch for univariate case
  if iscell(t)
    vals=t{k1};
  else
    vals=t;
  end
  
  bezier = ~isfield(s,'knots'); % if there are no knots --> bezier-form
  
  if ~bezier
    % spline switch for univariate case
    if iscell(s.knots)
      knots=s.knots{k1};
    else 
      knots= s.knots;
    end
  end;
     
  nums=size(e);
  e=reshape(e,nums(1),prod(nums(2:end))); % every column is a function 
  if bezier
    e=spl_eval_bez_curve(e,vals);
  else    
    e=spl_eval_spl_curve(e,knots,vals);
  end;
  [n1,n2]=size(e);                        
  e=reshape(e,[n1,nums(2:end)]);  % reorganize the parameter directions
  e=shiftdim(e,1);                % next direction to first place 
end;

if cdim ~= s.dimension % rational --> divide by last coordinate
  nums=size(e);
  e=reshape(e,nums(1),prod(nums(2:end)));
  for k1=1:s.dimension
    e(k1,:)=e(k1,:)./e(end,:);
  end;
  e(end,:)=[];
  nums(1)=s.dimension;
  e=reshape(e,nums);
end;
  
if cdim > 1  | s.parameter == 1 
  % more than one coordinate or only one parameter --> we need another shift
  e=shiftdim(e,1);
end;

%-----------------------------------------------------------------------------
% subroutine spl_eval_spl_curve
%-----------------------------------------------------------------------------
function V=spl_eval_spl_curve(p,u,t)
% evaluates spline curve with controllpoints p and knots u at values t

[p_num,f_num] = size(p);
degree = length(u) - p_num - 1;

V = zeros(0,f_num); % initialize to append
  
for x = t(:)' % for every value
  a=p;
  if ((u(degree+1) <= x) & (x <= u(end-degree)))       
    
    % find index ind with u_ind <= x < u_{ind+1} 
    ind = find(x == u); mult = length(ind);
    if mult == 0
      ind = find(x < u); ind = ind(1) - 1;
    elseif mult <= degree
      ind = ind(mult);
    else 				     % fix problem with first knot 
      ind = ind(mult) + (x == u(degree+1));  % having multiplicity > d
    end
    
    for k = degree-1:-1:mult
      J1 = [ind-k:ind-mult];
      w = ((x - u(J1))./(u(J1+k+1) - u(J1)))'*ones(1,f_num);
      a(J1,:) = w.*a(J1,:) + (1-w).*a(J1-1,:);
    end
    
    V = [V;a(ind-mult,:)];
    
  else
    fprintf('spl_eval_curve: Value %f not in support --> ignored\n',x)
  end
end
  
%-----------------------------------------------------------------------------
% subroutine spl_eval_bez_curve
%-----------------------------------------------------------------------------
function V=spl_eval_bez_curve(p,t)
% evaluates bezier curve with controllpoints p at values t

[p_num,f_num] = size(p);
t_num=length(t);
degree = p_num - 1;

% different substitutions for lower and upper values
V=zeros(t_num,f_num); % for better performance 

xl=t(find(t<(1/2)))';     
xu=t(find(t>=(1/2)))';  

b=binom(degree)*ones(1,f_num);

% upper half --> substitution y=(1-x)/x
a=b.*p;
y=(1-xu)./xu;
xpot=xu.^degree;
for k1=1:f_num
  V(find(t>=1/2),k1)=xpot.*polyval(a(:,k1),y);
end;

% lower half --> substitution y=x/(1-x)
a=b.*flipud(p);
y=xl./(1-xl);
xpot=(1-xl).^degree;
for k1=1:f_num
 V(find(t<1/2),k1)=xpot.*polyval(a(:,k1),y); 
end;
      
%-----------------------------------------------------------------------------
% subroutine binom
%-----------------------------------------------------------------------------
function v=binom(n)
% returns the binomial coefficients for n

v=1;
for k1=1:n/2
  v(k1+1)=v(k1)*(n+1-k1)/k1;
end;
if mod(n,2)
  v=[v,v(end:-1:1)]';
else
  v=[v,v(end-1:-1:1)]';
end;













