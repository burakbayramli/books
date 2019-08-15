function [p,e,t] = bsp03;
% Benhard cell, geometry data
% geometry adapted to that in bsp03g.m 
% which must be chosen in that form for initmesh.m
p1 = [0 1 1 0 0.5;
      0 0 1 1 0.5];
p = p1;     
for I = 1:11      
   p = [p,p1(:,3:5) + I];   
end
t1  = [1  2  15 14;   
      2  15 14  1;
     27  27 27 27];
t = t1;     
for I = 1:11
   t = [t,t1+I];
end      
%t2 = [2  3   16 15;   
%      3  16  15  2;
%      28 28  28 28];
      
e1 = [1; 2];
ea = e1;
for I = 1:11
   ea = [ea,e1+I];
end      
eb = [13; 26];
e1 = [26; 25];
ec = e1;
for I = 1:11
   ec = [ec,e1-I];
end      
ed = [14;1]
e = [ea,eb,ec,ed]
e = [e;zeros(5,size(e,2)];

J = find(p(1,:) > 3);
t4 = 2*ones(1,size(t,2));
for I = 1:size(t,2)
    if ~isempty(intersect(t(1:3,I),J))
    t4(I) = 1;
    end
end    
t = [t;t4];  

e1 = [15 8 3 ;
      8  3 1 ];
LE = size(e1,2); AUX = linspace(0,1,LE + 1);      
e1 = [e1;AUX(1:end-1);AUX(2:end);ones(1,LE)
      ones(1,LE);zeros(1,LE)];

e2 = [16 23 30 37 43 47;
      23 30 37 43 47 49];
LE = size(e2,2); AUX = linspace(0,1,LE + 1);      
e2 = [e2;AUX(1:end-1);AUX(2:end);2*ones(1,LE);
      ones(1,LE);zeros(1,LE)];
  
e3 = [49 45 39;
      45 39 31];
LE = size(e3,2); AUX = linspace(0,1,LE + 1);      
e3 = [e3;AUX(1:end-1);AUX(2:end);3*ones(1,LE);
      ones(1,LE);zeros(1,LE)];

e4 = [31 24 17 10 5 2;
      24 17 10  5 2 1];
LE = size(e4,2); AUX = linspace(0,1,LE + 1);      
e4 = [e4;AUX(1:end-1);AUX(2:end);4*ones(1,LE);
      ones(1,LE);2*ones(1,LE)];

e5 = [31 38 44;
      38 44 48];
LE = size(e5,2); AUX = linspace(0,1,LE + 1);      
e5 = [e5;AUX(1:end-1);AUX(2:end);5*ones(1,LE);
      2*ones(1,LE);zeros(1,LE)];

e6 = [48  46 42 36 29 22;
      46  42 36 29 22 15];
LE = size(e6,2); AUX = linspace(0,1,LE + 1);      
e6 = [e6;AUX(1:end-1);AUX(2:end);6*ones(1,LE);
      2*ones(1,LE);zeros(1,LE)];

e7 = [ 1 4 9;
       4 9 16];
LE = size(e7,2); AUX = linspace(0,1,LE + 1);      
e7 = [e7;AUX(1:end-1);AUX(2:end);7*ones(1,LE);
      2*ones(1,LE);zeros(1,LE)];

e  = [e1,e2,e3,e4,e5,e6,e7];

 