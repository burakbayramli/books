% May 2003, Jana Kosecka, George Mason University, GMU
% function [H1, H2] = proj_rectify(F,x1, x2, xdim, ydim)
% F - fundamental matrix between two views x2'*F*x1 = 0
% x1 - image coordinates of points in first view
% x2 - image coordinates of points in the second view
% xdim, ydim - dimensions of the image in pixel coordinates
% H1, H2 - rectifying transformations to be applied to image 1 and 2

% Algorith, 11.9 in Chapter 11, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
%
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003
%====================================================================

function [H1, H2] = projRectify(F,x1, x2, xdim, ydim)
  [uf, sf, vf] = svd(F); ep2 = uf(:,3); ep1 = vf(:,3); 
  ep2im = ep2/ep2(3);
  M = skew(ep2)'*F + ep2*rand(1,3);

  % take the epipole in the second view 
  Tr = [1 0 -xdim/2; 0 1 -ydim/2 ; 0 0 1];
  p2T = Tr*ep2im;
  % rotate the epipole to lie on the x-axis
  theta = atan(-p2T(2)/p2T(1));
  Rr =[cos(theta) -sin(theta) 0; sin(theta) cos(theta) 0 ; 0 0 1];
  p2R = Rr*p2T
  G = [1 0 0; 0 1 0; -1/p2R(1) 0 1];
  pim2r = G*p2R
  H2 = G*Rr*Tr;  % rectifying transformation for the second image

  %-----------------------------------------------------------------------
  % one method - compute matching homography - solve for unknown plane v so as
  % to minimize the disparity
  A = []; b = [];
  indr = 1:size(x1,2);
  for k =1:size(indr,2) % 
   i = indr(k);
   t1 = ep2(1); t2 = ep2(2); t3 = ep2(3);
   row1 = [-t2*x1(1,i)+t3*x1(1,i)*x2(2,i) ...
	   -t2*x1(2,i)+t3*x1(2,i)*x2(2,i) -t2+t3*x2(2,i)];
   row2 = [t1*x1(1,i)-t3*x1(1,i)*x2(1,i) ...
	   t1*x1(2,i)-t3*x1(2,i)*x2(1,i) t1-t3*x2(1,i)];
   A = [A; row1; row2];
   rhs1 = M(2,1)*x1(1,i)+ M(2,2)*x1(2,i)+M(2,3)-M(3,1)*x1(1,i)*x2(2,i)...
	  -M(3,2)*x1(2,i)*x2(2,i)- M(3,3)*x2(2,i);
   rhs2 = -M(1,1)*x1(1,i)-M(1,2)*x1(2,i)-M(1,3)+M(3,1)*x1(1,i)*x2(1,i)...
	  +M(3,2)*x1(2,i)*x2(1,i)+ M(3,3)*x2(1,i);
   b = [b; rhs1; rhs2];
  end;
 aa = A\b;
 H = M + ep2*aa';
 H1 = H2*H;
  

