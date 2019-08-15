function spline = spl_knot(spline,t)

% function spline = spl_knot(spline,t)
% add knots t{i} 
% spline.points: control point array a of
%    dimension prod n_i x coordinate-number
% spline.knots{i}: knots u_i(1:n_i+d_i+1)

if ~isfield(spline,'knots'); % bezier form
   fprintf(['\n spl_knot: ' ... 
      'no knots for Bezier form \n']); 

elseif ~iscell(spline.knots); 
   % univariate

   % get parameters
   a = spline.points;
   u = spline.knots(:); 
   [la,dim] = size(a);
   lu = length(u);
   d = lu - la - 1;
   uleft = u(d+1); uright = u(end-d);
   e = ones(1,dim);

   for v = t(:)';

      % check if t lies in parameter interval
      if (v < uleft | uright < v)
%         fprintf(['\n spl_knot: ' ... 
%	    'knot out of range \n']);
      else
         
	 % indices for insertion recursion
         % J = [k,l] with (*): u_j < t < u_{j+d}
	 J = find(u(1:end-d)<v & v<u(1+d:end)); 
         if isempty(J);
            % condition (*) cannot be met 
%            fprintf(['spl_knot: ' ... 
%	       '\n multiplicity>degree \n']);
         else
            k=J(1); l=J(end);
            
	    % form convex combinations
            c = ((v-u(J))./(u(J+d)-u(J)))*e;
            b = c.*a(J,:)+(1-c).*a(J-1,:); 
            a = [a(1:k-1,:); b; a(l:la,:)];
            u = [u(1:l); v; u(l+1:lu)]; 
            
	    % remove irrelevant boundary knot
            % and corresponding control point 
            % if t is equal to an endpoint 
            if v <= uleft; 
               a = a(2:end,:); 
               u = u(2:end);
            elseif uright <= v  
               a = a(1:end-1,:); 
               u = u(1:end-1);
            else;
               la = la+1; lu = lu+1;
            end;
         end;
      end;
   end;

   % results
   spline.points = a;
   spline.knots = u; 

else; % multivariate

   for i=1:length(spline.knots)
   
      % get parameters 
      si.knots = spline.knots{i}(:);
      N = size(spline.points);
      si.points = reshape(spline.points, ... 
         N(1),prod(N(2:end)));
 
      % insert knots for parameter i
      si = spl_knot(si,t{i});

      % reformat result
      spline.knots{i} = si.knots;
      [n1,n2] = size(si.points);
      spline.points = reshape(si.points,[n1 ...
         N(2:end)]);

      % move next index to first position
      spline.points = shiftdim(spline.points,1);
   end;

   % extra shift if dimension > 1
   if length(N) > length(spline.knots);
      spline.points = shiftdim(spline.points,1);
   end;
end;
