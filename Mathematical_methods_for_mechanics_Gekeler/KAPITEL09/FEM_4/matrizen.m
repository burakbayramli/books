function [KK,MM,CC] = matrizen(p,e,t);
% lineare Dreieckelemente

N = size(p,2); KK = sparse(N,N); MM = KK; CC = KK; Y = p(2,:);
for I = 1:size(t,2)
    J = t(1:3,I);
   [KE,ME,BE,ecode] = drlell(p(1,J),p(2,J));
   KK(J,J) = KK(J,J) + KE;                  %ev. + RHO*ME;
   MM(J,J) = MM(J,J) + ME;
   Y21     = Y(J(2))-Y(J(1)); Y31 = Y(J(3))-Y(J(1)); Y32 = Y(J(3))-Y(J(2));
   CC(J,J) = CC(J,J) + ones(3,1)*[- Y32,  Y31, - Y21]/6;
end
