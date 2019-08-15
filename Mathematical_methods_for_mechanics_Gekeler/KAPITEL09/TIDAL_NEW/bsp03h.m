function [RDU,RDV,RDZ] = bsp03h(p,e,V,T,A,PERIOD,H,L);
% Boundary data for channel
g = 9.81; 
omga = 2*pi/PERIOD; N = size(p,2);
AUX = 2*pi*(T - L/sqrt(g*H))/PERIOD;
Z_EXACT = A*sin(AUX); % exact at for open end
U_RIGHT = Z_EXACT*sqrt(g)/sqrt(H + Z_EXACT);

% -- U-Teil -------------------------
I = find(e(5,:) == 2); LI = length(I); % seg.-nr. 2: right boundary
E = [e(1,I), e(2,I(LI))];

RSIDE = 2;
switch RSIDE
case 1 % open boundary 1, exact
   if T <= L/sqrt(g*H)
      RDU_2 = [E;zeros(1,LI+1)]; %u exact with numerical z 
   else
      RDU_2 = [E;U_RIGHT*ones(1,LI+1)];
   end   
case 2 % open boundary, 2, nearly same results
   % right side with numerical data
   AUX = p(3,E) + V(3,E);
   RDU_2 = [E;sqrt(g)*V(3,E)./sqrt(AUX)]; %u exact with numerical z 
%   RDU_2a = [E;U_RIGHT*ones(1,LI+1)];
%   DIFF = RDU_2a(2,:) - RDU_2(2,:);
%   WERT1 = RDU_2(2,:)
%   WERT2 = RDU_2a(2,:)
case 3 % closed boundary
   RDU_2 = [E;zeros(1,LI+1)];
end
RDU = RDU_2;
% -- V-Teil --------------------
I = find(e(5,:) == 1); LI = length(I); % Rand 1
RDV_1 = [e(1,I);zeros(1,LI)];
I = find(e(5,:) == 2); LI = length(I); % Rand 2
RDV_2 = [e(1,I);zeros(1,LI)];
I = find(e(5,:) == 3); LI = length(I); % Rand 3
RDV_3 = [e(1,I);zeros(1,LI)];
I = find(e(5,:) == 4); LI = length(I); % Rand 4
RDV_4 = [e(1,I);zeros(1,LI)];
RDV = [RDV_1,RDV_2,RDV_3,RDV_4];

% -- Z-Teil ----------------------
I = find(e(5,:) == 4); LI = length(I); % seg.-nr. 4: inlet 
E = [e(1,I),e(2,I(LI))];
RDZ_4 = [e(1,I),e(2,I(LI));A*sin(omga*T)*ones(1,LI+1)]; 
RDZ = RDZ_4;
