function [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = bsp09h(p,e,p1,parmeter)
% Example: BACK FACING STEP
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 
% Seg.-nr. of ordered boundary: 
% SEGNR  = [6 7 8 9 14 19 24 29 34 39 44 45 42 43 38 33 28 23 18 13 3 5];

M = size(p,2);
U0 = parmeter(3); scale_factor = parmeter(4);

H = 0.036*scale_factor; % for Z
% -- BOUNDARY CONDITIONS for U and V (no-slip boundary conditions) 
I  = find(e(5,:) == 6); LI = length(I); % Randsegment 6 (left+right closed  )
AUX = [e(1,I),e(2,I(LI)); U0*ones(1,LI+1)]; % inflow
RDU6 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
RDV6 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 7); LI = length(I); % Randsegment 7 (right closed)
%AUX = [e(2,I); U0*ones(1,LI)];
%RDU7 = [AUX, [e(8,I); U0*ones(1,LI)]];

RDU7 = [e(2,I),e(8,I); zeros(1,2*LI)]; RDV7 = RDU7;

I  = find(e(5,:) == 8); LI = length(I); % Randsegment 8 (open/open)
AUX = [e(1,I(2:LI)); zeros(1,LI-1)];
RDU8 = [AUX, [e(8,I); zeros(1,LI)]];    RDV8 = RDU8;

I  = find(e(5,:) == 9); LI = length(I); % Randsegment 9 (closed/open)
AUX = [e(1,I); zeros(1,LI)];
RDU9 = [AUX, [e(8,I); zeros(1,LI)]];    RDV9 = RDU9;

I  = find(e(5,:) == 14); LI = length(I); % Randsegment 14 (closed/open)
AUX = [e(1,I); zeros(1,LI)];
RDU14 = [AUX, [e(8,I); zeros(1,LI)]];   RDV14 = RDU14;

I  = find(e(5,:) == 19); LI = length(I); % Randsegment 19 (closed/open)
AUX = [e(1,I); zeros(1,LI)];
RDU19 = [AUX, [e(8,I); zeros(1,LI)]];    RDV19 = RDU19;

I  = find(e(5,:) == 24); LI = length(I); % Randsegment 24 (closed/open)
AUX = [e(1,I); zeros(1,LI)];
RDU24 = [AUX, [e(8,I); zeros(1,LI)]];    RDV24 = RDU24;

I  = find(e(5,:) == 29); LI = length(I); % Randsegment 29 (closed/open)
AUX = [e(1,I); zeros(1,LI)];
RDU29 = [AUX, [e(8,I); zeros(1,LI)]];    RDV29 = RDU29; 

I  = find(e(5,:) == 34); LI = length(I); % Randsegment 34 (closed/open)
AUX = [e(1,I); zeros(1,LI)];
RDU34 = [AUX, [e(8,I); zeros(1,LI)]];    RDV34 = RDU34;

I  = find(e(5,:) == 39); LI = length(I); % Randsegment 39 (closed/open)
AUX = [e(1,I); zeros(1,LI)];
RDU39 = [AUX, [e(8,I); zeros(1,LI)]];    RDV39 = RDU39;

I  = find(e(5,:) == 44); LI = length(I); % Randsegment 44 (closed/closed)
AUX = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
RDU44 = [AUX, [e(8,I); zeros(1,LI)]];    RDV44 = RDU44;  

I  = find(e(5,:) == 45); LI = length(I); % Randsegment 45 (open/closed)
AUX = [e(2,I); zeros(1,LI)];             % outflow 
RDV45 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 42); LI = length(I); % Randsegment 42 (open/open)
AUX = [e(1,I(2:LI)); zeros(1,LI-1)];     % outflow 
RDV42 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 43); LI = length(I); % Randsegment 43 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU43 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV43 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 38); LI = length(I); % Randsegment 38 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU38 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV38 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 33); LI = length(I); % Randsegment 33 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU33 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV33 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 28); LI = length(I); % Randsegment 28 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU28 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV28 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 23); LI = length(I); % Randsegment 23 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU23 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV23 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 18); LI = length(I); % Randsegment 18 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU18 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV18 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 13); LI = length(I); % Randsegment 13 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU13 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV13 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU3 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV3 = [AUX, [e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 5); LI = length(I); % Randsegment 5 (closed/open)
AUX = [e(1,I); U0*ones(1,LI)];
RDU5 = [AUX, [e(8,I); U0*ones(1,LI)]];
AUX = [e(1,I); zeros(1,LI)];
RDV5 = [AUX, [e(8,I); zeros(1,LI)]];

RDUA = [RDU6,RDU7,RDU8,RDU9,RDU14,RDU19,RDU24,RDU29,RDU34,RDU39,RDU44];
RDUB = [RDU43,RDU38,RDU33,RDU28,RDU23,RDU18,RDU13,RDU3,RDU5];
RDU = [RDUA,RDUB];

RDVA = [RDV6,RDV7,RDV8,RDV9,RDV14,RDV19,RDV24,RDV29,RDV34,RDV39,RDV44];
RDVB = [RDV45,RDV42,RDV43,RDV38,RDV33,RDV28,RDV23,RDV18,RDV13,RDV3,RDV5];
RDV = [RDVA,RDVB];

I  = find(e(5,:) == 45); LI = length(I); % Randsegment 9 outflow
RDP = [e(1,I(1)); 0];

% -- Loads ---------------------
FU = zeros(size(p,2)+size(p1,2),1); FV = FU;

% -- BOUNDARY CONDITIONS FOR STREAM FUNCTION
I  = find(e(5,:) == 6); LI = length(I); % Randsegment 6 inflow
[U,V,Z] = inflow(p(1,e(1,I)),p(2,e(1,I)),parmeter);
RDZ6 = [e(1,I); Z];
[U,V,Z]= inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M),parmeter);
RDZ6 = [RDZ6,[e(8,I); Z]];

I  = find(e(5,:) == 7); LI = length(I); % Randsegment 7
RDZ7 = [e(1,I), e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 8); LI = length(I); % Randsegment 8
RDZ8 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 9); LI = length(I); % Randsegment 9
RDZ9 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 14); LI = length(I); % Randsegment 14
RDZ14 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 19); LI = length(I); % Randsegment 19
RDZ19 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 24); LI = length(I); % Randsegment 24
RDZ24 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 29); LI = length(I); % Randsegment 29
RDZ29 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 34); LI = length(I); % Randsegment 34
RDZ34 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 39); LI = length(I); % Randsegment 39
RDZ39 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 44); LI = length(I); % Randsegment 44
RDZ44 = [e(1,I),e(2,I(LI)),e(8,I); zeros(1,2*LI+1)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
I  = find(e(5,:) == 43); LI = length(I); % Randsegment 43
RDZ43 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];
I  = find(e(5,:) == 38); LI = length(I); % Randsegment 38
RDZ38 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];
I  = find(e(5,:) == 33); LI = length(I); % Randsegment 33
RDZ33 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];
I  = find(e(5,:) == 28); LI = length(I); % Randsegment 28
RDZ28 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];
I  = find(e(5,:) == 23); LI = length(I); % Randsegment 23
RDZ23 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];
I  = find(e(5,:) == 18); LI = length(I); % Randsegment 18
RDZ18 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];
I  = find(e(5,:) == 13); LI = length(I); % Randsegment 13
RDZ13 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ3 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];
I  = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDZ5 = [e(1,I),e(8,I); U0*H*ones(1,2*LI)];


RDZA = [RDZ6,RDZ7,RDZ8,RDZ9,RDZ14,RDZ19,RDZ24,RDZ29,RDZ34,RDZ39,RDZ44];
RDZB = [RDZ43,RDZ38,RDZ33,RDZ28,RDZ23,RDZ18,RDZ13,RDZ3,RDZ5];
RDZ = [RDZA,RDZB];
RCZ = []; % Neumann boundary conditions

function [U,V,Z]= inflow(X,Y,parmeter);
   U0 = parmeter(3); scaled = parmeter(5);
   LX = length(X); 
   U = U0*ones(1,LX); V = zeros(1,LX);
   % Z = int_0^y u(y)dy
   switch scaled
   case 0 % unscaled
      Z = U0*(Y - 0.02);  %unscaled
   case 1 % scaled
      Z = U0*(Y - 1);       % scaled
   case 2 % scaled
      Z = U0*(Y - 1);       % scaled
   end