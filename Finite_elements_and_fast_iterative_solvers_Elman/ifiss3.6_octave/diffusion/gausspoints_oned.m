function [oneg,onew]=gausspoints_oned(ngpt) 
%GAUSSPOINTS_ONED constructs one-dimensional Gauss Point Rule
% [oneg,onew] = gausspoints_oned(ngpt);
%  input
%   ngpt    number of points (exact for polynomials of degree: 2*ngpts-1) 
%  output
%   oneg    1D Gaussian Points in (-1,1)
%   onew    Weights in 1D
%    IFISS function: DJS; 2 January 2011.
%  Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
if ngpt==10,
ng=10;  % 10 Gauss point rule
oneg(1)= 0.1488743389816312108848260;
oneg(2)=-0.1488743389816312108848260;
oneg(3)= 0.4333953941292471907992659;
oneg(4)=-0.4333953941292471907992659;
oneg(5)= 0.6794095682990244062343274;
oneg(6)=-0.6794095682990244062343274;
oneg(7)= 0.8650633666889845107320967;
oneg(8)=-0.8650633666889845107320967;
oneg(9)= 0.9739065285171717200779640;
oneg(10)=-0.9739065285171717200779640;
onew(1)=0.2955242247147528701738930;
onew(2)=0.2955242247147528701738930;
onew(3)=0.2692667193099963550912269;
onew(4)=0.2692667193099963550912269;
onew(5)=0.2190863625159820439955349;
onew(6)=0.2190863625159820439955349;
onew(7)=0.1494513491505805931457763;
onew(8)=0.1494513491505805931457763;
onew(9)=0.0666713443086881375935688;
onew(10)=0.0666713443086881375935688;
elseif ngpt==7,
ng=7;  % 7 Gauss point rule
       oneg(1)=0.949107912342759;
       oneg(2)=-0.949107912342759;
       oneg(3)=0.741531185599394;
       oneg(4)=-0.741531185599394;
       oneg(5)=0.405845151377397;
       oneg(6)=-0.405845151377397;
       oneg(7)=0;
       onew(1)=0.129484966168870;
       onew(2)=0.129484966168870;
       onew(3)=0.279705391489277;
       onew(4)=0.279705391489277;
       onew(5)=0.381830050505119;
       onew(6)=0.381830050505119;
       onew(7)=0.417959183673469;
elseif ngpt==4  
ng=4; % 4 Gauss point rule
      ggptI=sqrt((3-2*sqrt(6/5))/7);  wtI=(18+sqrt(30))/36;
      ggptII=sqrt((3+2*sqrt(6/5))/7); wtII=(18-sqrt(30))/36;     
      oneg(1) =  ggptI;  onew(1) = wtI; 
      oneg(2) = -ggptI;  onew(2) = wtI; 
      oneg(3) =  ggptII; onew(3) = wtII;
      oneg(4) = -ggptII; onew(4) = wtII;
elseif ngpt==3  
ng=3; % 3 Gauss point rule 
      gpt=sqrt(0.6); 
      oneg(1) = -gpt; onew(1) = 5/9; 
      oneg(2) = gpt;  onew(2) = 5/9;
      oneg(3) = 0.0;  onew(3) = 8/9;
elseif ngpt==2  
ng=2; % 2 Gauss point rule 
      gpt=1/sqrt(3); 
      oneg(1) = -gpt; onew(1) = 1.0; 
      oneg(2) = gpt;  onew(2) = 1.0;
elseif ngpt==1
ng=1; % 1 Gauss point rule  
      oneg(1) = 0.0;  onew(1) = 2.0;    
else
error('Gaussian integration rule not yet implemented!')
end
return
