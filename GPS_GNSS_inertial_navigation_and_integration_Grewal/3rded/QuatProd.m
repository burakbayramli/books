function QuatResult = QuatProd(QuatLeft,QuatRight)
%
% Computes the product of two quaternions as column 4-vectors
% 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
%
QuatResult = [QuatLeft(1)*QuatRight(1) - QuatLeft(2)*QuatRight(2) - QuatLeft(3)*QuatRight(3) - QuatLeft(4)*QuatRight(4);
              QuatLeft(2)*QuatRight(1) + QuatLeft(1)*QuatRight(2) - QuatLeft(4)*QuatRight(3) + QuatLeft(3)*QuatRight(4);
              QuatLeft(3)*QuatRight(1) + QuatLeft(4)*QuatRight(2) + QuatLeft(1)*QuatRight(3) - QuatLeft(2)*QuatRight(4);
              QuatLeft(4)*QuatRight(1) - QuatLeft(3)*QuatRight(2) + QuatLeft(2)*QuatRight(3) + QuatLeft(1)*QuatRight(4)];
end;
