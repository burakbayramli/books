% Algorithm 11.8 
% estimate focal length using absolute quadric constrainst
% the remaining paratemters of calibration matrix are assumed known
% Described in Chapter 11, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003

% function Omega = quadric_linear_f(P,varargin);
% P - projection matrices
% Omega - absolute quadric

function Omega = quadric_linear_f(P,varargin);

 if ~isempty(varargin) ar = varargin{1}; else
    ar = 1;
 end;
  ar ;
  PC = P;
  FRAMES = size(PC,3);
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % case 1 only focal length is unknown
  % set up the constraints on absolute quadric
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  for j=2:FRAMES
   i = 4*(j-2) + 1;
   D(i,:) = 2*row_quadric_f(PC(1,:,j), PC(2,:,j));
   D(i+1,:) = 2*row_quadric_f(PC(1,:,j), PC(3,:,j));
   D(i+2,:) = 2*row_quadric_f(PC(2,:,j), PC(3,:,j));
   D(i+3,:) = (ar^2)*row_quadric_f(PC(1,:,j), PC(1,:,j))-row_quadric_f(PC(2,:,j), PC(2,:,j));
  end;
  A = D(:,1:5);
  b = -D(:,6);
  t = A\b;
  ttt = t/norm(t);

if 0
[u2, s2, v2] = svd(D);
eigenvaluesofD = s2(5:6,5:6)
%  randDD = rank(D);
vv = v2(:,6)/v2(6,6);
t = vv(1:5);
tttt = t/norm(t);
end

ff = [t(1), t(2), t(3), t(4), t(5)];
norm(t(2:4))^2;
     
Omega = [t(1)  0    0   t(2);
            0   t(1)  0   t(3);
            0    0    1   t(4);
	     t(2) t(3) t(4) t(5)]

[u1,s1,v1] = svd(Omega);
s1(4,4) = 0;
Omega = u1*s1*v1'
 
