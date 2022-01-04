


function  u = heat6(x,y,t)
% function mfile to calculate a solution of the heat equation consisting
% of two Gaussian humps. Requires parameters Q1, Q2, a1, b1, a2, b2.

     global centers Q

     a1 = centers(1); b1 = centers(2); a2 = centers(3); b2 = centers(4);
     Q1 = Q(1); Q2 = Q(2);

     s1 = .5 ; s2 = 2;

     hump1 = (Q1/(4*pi*(s1+t)))*exp( -((x-a1).^2 + (y-b1).^2 )./(4*(s1+t)) );
     hump2 = (Q2/(4*pi*(s2+t)))*exp( -((x-a2).^2 + (y-b2).^2 )./(4*(s2+t)) );

     u = hump1 + hump2 ;
