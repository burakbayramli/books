function bcweights = m_bcscale(xy,domain,qmethod)
%M_BCSCALE diagonal scaling for boundary-adjusted LSC 
%   bcweights = m_bcscale(xy,domain,qmethod);
%   input
%          xy         grid coordinates
%          domain     domain identifier
%          qmethod    discretization identifier
%   output
%          bcweights  diagonal matrix of scalings used to deemphasize
%                     tangential components of LSC preconditioner near
%                     boundaries
%
%   IFISS function: HCE; 21 August 2012.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage

% Deemphasize tangential component near all boundaries
% Identify indices at which diagonal scaling should be epsilon
% For qmethod>1 (Q2 velocities):  nodes involved are on both element
%                                 boundaries and interiors
% For qmethod<1 (Q1 velocities):  nodes involved are only on element
%                                 boundaries

% For vertical boundaries, offset by nv
nv = size(xy,1);

%% Cavity
if domain==1 || domain==7,
    x_sorted = sort(xy(:,1)); 
    xmin = x_sorted(1); xmax = x_sorted(length(x_sorted));
    y_sorted = sort(xy(:,2)); 
    ymin = y_sorted(1); ymax = y_sorted(length(y_sorted));
% Near left boundary    
   x1 = xmin;
   x2_index = find(x_sorted>x1,1);
   x2 = x_sorted(x2_index);
   if qmethod <= 1,
      left = nv + find( (xy(:,1)==x2) & (xy(:,2)>ymin) & (xy(:,2)<ymax) );
   else
      x3_index = find(x_sorted>x2,1);
      x3 = x_sorted(x3_index);
      left = nv + find( (xy(:,1)==x2 | xy(:,1)==x3) & ...
                        (xy(:,2)>ymin) & (xy(:,2)<ymax) );
   end        
% Near top boundary
   y1 = ymax;
   y2_index = find(y_sorted<y1,1,'last');  %max( find(y_sorted<y1) );
   y2 = y_sorted(y2_index);
   if qmethod <= 1,
      top = find( (xy(:,2)==y2) & (xy(:,1)>xmin) & (xy(:,1)<xmax) );
   else
      y3_index = find(y_sorted<y2,1,'last');  %max( find(y_sorted<y2) );
      y3 = y_sorted(y3_index);
      top = find( (xy(:,2)==y2 | xy(:,2)==y3) & ...
                  (xy(:,1)>xmin) & (xy(:,1)<xmax) );
   end
% Near right boundary
   x1 = xmax;
   x2_index = find(x_sorted<x1,1,'last');
   x2 = x_sorted(x2_index);
   if qmethod <= 1,
      right = nv + find( (xy(:,1)==x2) & (xy(:,2)>ymin) & (xy(:,2)<ymax) );
   else
      x3_index = find(x_sorted<x2,1,'last');
      x3 = x_sorted(x3_index);
      right = nv + find( (xy(:,1)==x2 | xy(:,1)==x3) & ...
                         (xy(:,2)>ymin) & (xy(:,2)<ymax) );
   end           
% Near bottom boundary
   y1 = ymin;
   y2_index = find(y_sorted>y1,1); %min( find(y_sorted>y1) );
   y2 = y_sorted(y2_index);
   if qmethod <= 1,
      bottom = find( (xy(:,2)==y2) & (xy(:,1)>xmin) & (xy(:,1)<xmax) );
   else
      y3_index = find(y_sorted>y2,1);  %min( find(y_sorted>y2) );
      y3 = y_sorted(y3_index);
      bottom = find( (xy(:,2)==y2 | xy(:,2)==y3) & ...
                     (xy(:,1)>xmin) & (xy(:,1)<xmax) );
   end
%
   bcs = [left;top;right;bottom];
%   
%% Step
elseif domain==3,
% Near inflow boundary
   infl_size = length(find(xy(:,1)==-1));
   if qmethod>1, 
      infl_offset = 2*infl_size + 1;
   else
      infl_offset = infl_size +1;
   end
   h = xy(infl_offset,1) - xy(1,1);
   infl = nv + find( (xy(:,1)>-1) & (xy(:,1)<=-1+h) & ...
                     (xy(:,2)>0)  & (xy(:,2)<1) );

% Near vertical part of step
   bottom_left_index = find( (xy(:,1)==0) & (xy(:,2)==-1) );
   if qmethod>1,
      vert_offset = 2*sum(xy(:,1)==0);
   else
      vert_offset = sum(xy(:,1)==0);
   end
   h = xy(bottom_left_index+vert_offset,1) - xy(bottom_left_index,1);
   step_vert = nv + find( (xy(:,1)>0)  & (xy(:,1)<=h) & ...
                          (xy(:,2)>-1) & (xy(:,2)<=0) );

% Deemphasize horizontal component near horizontal boundaries
% Near top
   if qmethod>1,
      h = xy(infl_size,2)-xy(infl_size-2,2);
   else
      h = xy(infl_size,2)-xy(infl_size-1,2);
   end
   x_max = max(xy(:,1));
   top = find( (xy(:,1)>-1) & (xy(:,1)<x_max) & ...
               (xy(:,2)>=1-h) & (xy(:,2)<1) );

% Near bottom
   if qmethod>1,
      h = xy(bottom_left_index+2,2) - xy(bottom_left_index,2);
   else
      h = xy(bottom_left_index+1,2) - xy(bottom_left_index,2);
   end
   bottom = find( (xy(:,1)>0) & (xy(:,1)<x_max) & ...
                  (xy(:,1)>0) & (xy(:,2)<=-1+h) ); 

% Near horizontal part of step
   if qmethod>1,
      h = xy(3,2)-xy(1,2);
   else
      h = xy(2,2)-xy(1,2);
   end
   step_hor = find( (xy(:,1)>-1) & (xy(:,1)<=0) & ...
                    (xy(:,2)>0) & (xy(:,2)<=h) );
%
   bcs = [infl;step_vert;top;bottom;step_hor];

%% Obstacle domain
elseif domain==4,
    x_sorted = sort(xy(:,1)); 
    xmin = x_sorted(1); xmax = x_sorted(length(x_sorted));
    y_sorted = sort(xy(:,2)); 
    ymin = y_sorted(1); ymax = y_sorted(length(y_sorted));
% Near left (inflow)
   x1 = xmin;
   x2_index = find(x_sorted>x1,1);
   x2 = x_sorted(x2_index);
   if qmethod <= 1,
      infl = nv + find( (xy(:,1)==x2) & (xy(:,2)>ymin) & (xy(:,2)<ymax) );
   else
      x3_index = find(x_sorted>x2,1);
      x3 = x_sorted(x3_index);
      infl = nv + find( (xy(:,1)==x2 | xy(:,1)==x3) & ...
                        (xy(:,2)>ymin) & (xy(:,2)<ymax) );
   end
% Near bottom.  Find second smallest y-value, and if necessary, third
% smallest
   y1 = ymin;
   y2_index = find(y_sorted>y1,1); %min( find(y_sorted>y1) );
   y2 = y_sorted(y2_index);
   if qmethod <= 1,
      bottom = find( (xy(:,2)==y2) & (xy(:,1)>xmin) & (xy(:,1)<xmax) );
   else
      y3_index = find(y_sorted>y2,1);  %min( find(y_sorted>y2) );
      y3 = y_sorted(y3_index);
      bottom = find( (xy(:,2)==y2 | xy(:,2)==y3) & ...
                     (xy(:,1)>xmin) & (xy(:,1)<xmax) );
   end
% Near top.  Find second largest y-value, and if necessary, third largest 
   y1 = ymax;
   y2_index = find(y_sorted<y1,1,'last');  %max( find(y_sorted<y1) );
   y2 = y_sorted(y2_index);
   if qmethod <= 1,
      top = find( (xy(:,2)==y2) & (xy(:,1)>xmin) & (xy(:,1)<xmax) );
   else
      y3_index = find(y_sorted<y2,1,'last');  %max( find(y_sorted<y2) );
      y3 = y_sorted(y3_index);
      top = find( (xy(:,2)==y2 | xy(:,2)==y3) & ...
                  (xy(:,1)>xmin) & (xy(:,1)<xmax) );
   end
%
% Near obstacle
   obs_left   = 1.75;   obs_right  = 2.25;
   obs_bottom = -.25;   obs_top    =  .25;
%
   x_sorted = sort(xy(:,1));
   xright_indices = find(x_sorted>obs_right);
   xp1 = x_sorted(xright_indices(1));
   hright = xp1-obs_right;
   xleft_indices = find(x_sorted<obs_left);
   xm1 = x_sorted(xleft_indices(length(xleft_indices)));
   hleft = obs_left-xm1;
   if qmethod>1,
      xright_indices = find(x_sorted>xp1);
      xp2 = x_sorted(xright_indices(1));
      hright = xp2-obs_right;
      xleft_indices = find(x_sorted<xm1);
      xm2 = x_sorted(xleft_indices(length(xleft_indices)));
      hleft = obs_left-xm2;
   end
%
   y_sorted = sort(xy(:,2));
   ytop_indices = find(y_sorted>.25);
   yp1 = y_sorted(ytop_indices(1));
   htop = yp1 - obs_top;
   ybottom_indices = find(y_sorted<obs_bottom);
   ym1 = y_sorted(ybottom_indices(length(ybottom_indices)));
   hbottom = obs_bottom - ym1;
   if qmethod>1,
      ytop_indices = find(y_sorted>yp1);
      yp2 = y_sorted(ytop_indices(1));
      htop = yp2 - obs_top;
      ybottom_indices = find(y_sorted<ym1);
      ym2 = y_sorted(ybottom_indices(length(ybottom_indices)));
      hbottom = obs_bottom - ym2;
   end
%
   left_obs   = find( (xy(:,1)>=obs_left-hleft) & (xy(:,1)<obs_left) & ...
                      (xy(:,2)>=obs_bottom) & (xy(:,2)<=obs_top) ) + nv;
   right_obs  = find( (xy(:,1)>obs_right) & (xy(:,1)<=obs_right+hright) & ...
                      (xy(:,2)>=obs_bottom) & (xy(:,2)<=obs_top) ) + nv;
   bottom_obs = find( (xy(:,2)>=obs_bottom-hbottom) & (xy(:,2)<obs_bottom) & ...
                      (xy(:,1)>=obs_left) & (xy(:,1)<=obs_right) );
   top_obs    = find( (xy(:,2)>obs_top) & (xy(:,2)<=obs_top+htop) & ...
                      (xy(:,1)>=obs_left) & (xy(:,1)<=obs_right) );
%
   bcs = [infl;bottom;top; left_obs;right_obs;bottom_obs;top_obs];
end
%
%
%%
% Return diagonal matrix with scaling eps=.1 in entries where scaling
% is to be done and ones in other entries
weights = ones(2*nv,1);
weights(bcs) = .1; 
bcweights = spdiags(weights,0,2*nv,2*nv);
