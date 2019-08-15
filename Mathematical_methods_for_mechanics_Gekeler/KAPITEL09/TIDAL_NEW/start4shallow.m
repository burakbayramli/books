function [p,e,t,waterdepth] = start4shallow(FF1,REFINE,JIGGLE) 
% makes mesh for triangular elements
% E.W. Gekeler, Release 2.03.10
 
if nargin == 2, JIGGLE = 0; end

[p,e,t,waterdepth] = feval(FF1); % first mesh
for J = 1:REFINE
   disp(' Refinemesh ')
   [p,e,t,waterdepth] = mesh01_t_sea([],p,e,t,waterdepth);
   if JIGGLE == 1
     % p       = mesh10(p,e,t,4); % Jigglemesh NO INTERPOLATION!!!
       t       = mesh03(p,t,0);   % replace long edges
   end  
end
