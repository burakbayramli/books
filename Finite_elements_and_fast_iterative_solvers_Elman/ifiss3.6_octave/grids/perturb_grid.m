function xy = perturb_grid(ev,ebound,xy)
%PERTURB_GRID perturbed bilinear element grid generator
%   xy = perturb_grid(ev,ebound,xy);
%   input
%          ev      element mapping matrix  
%          ebound  element boundary mapping matrix
%          xy      vertex coordinate vector
%   output
%          xy      perturbed vertex coordinate vector
%
%   IFISS function: NSW, 21 December 2005.
%   Copyright © 2005 N.S. Watson.
%   $Revision: 0.04 $  $Date: 21/12/2005 10:36:03 $

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        The contents of this file are licensed under the Open Software        %
%         License 2.1 or Academic Free License 2.1                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


alpha=default('grid perturbation: 0-1 (default 0, no perturbation)',0);

if alpha>0 && alpha<=1          % if there is perturbation and it is a valid number.
nvtx=length(xy);
h=abs(xy(1,1)-xy(2,1));         %Future Work: Works for stretched grids, but not so well.
intvtx = zeros(nvtx,1);
nv = max(ev);
   for vrtx = 1:nv              %loop finds all internal nodes.
       [el,node]=find(ev==vrtx);
       if length(el)==4
           intvtx(vrtx)=1;
       end
   end

p_x=intvtx.*(rand(nvtx,1)-.5);
p_y=intvtx.*(rand(nvtx,1)-.5);
while length(find(abs(p_x)+abs(p_y)>=0.5))~=0       %finds all of the purturbations
    where = find(abs(p_x)+abs(p_y)>=0.5);           %that lie out side the 1-norm ball
    len=length(where);                              %radius 0.5 and puts them inside the
    p_x(where)=rand(len,1);                         %ball (centered at the node)
    p_y(where)=rand(len,1);
end

xy = [xy(:,1)-alpha*h*p_x,xy(:,2)-alpha*h*p_y];
end

return
