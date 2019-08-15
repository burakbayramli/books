function B = cauchy4w(p,e);

% Normalableitungen von W
% CAUCHY-Randbedingungen ----------------
I = find(e(5,:) == 1); %Boundary 1
RCW1 = e(1:2,I); x = p(1,e(1,I)); y = p(2,e(1,I));
[wx,wy] = randw(x,y);
RCW1 = [RCW1;-wy];

I = find(e(5,:) == 2);  %Boundary 2
RCW2 = e(1:2,I); x = p(1,e(1,I)); y = p(2,e(1,I));
[wx,wy] = randw(x,y);
RCW2 = [RCW2;wx];

I = find(e(5,:) == 3); %Boundary 3
RCW3 = e(1:2,I); x = p(1,e(1,I)); y = p(2,e(1,I));
[wx,wy] = randw(x,y);
RCW3 = [RCW3;wy];

I = find(e(5,:) == 4); %Boundary 4
RCW4 = e(1:2,I); x = p(1,e(1,I)); y = p(2,e(1,I));
[wx,wy] = randw(x,y);
RCW4 = [RCW4;-wx];

RCW = [RCW1,RCW2,RCW3,RCW4];

B = zeros(size(p,2),1);
for I = 1:size(RCW,2)
  K        = RCW(1:2,I);
  [ME,BE]  = ralell(p(1,K),p(2,K));
  B(K)     = B(K)   + RCW(3,I)*BE;
end

function [wx,wy] = randw(x,y)
wx = 16*(12*x-6).*(y-y.^2).^2+32*(x-x.^2).*(6*y.^2-6*y+1).*(1-2*x);
wy = 32*(6*x.^2-6*x+1).*(y-y.^2).*(1-2*y)+16.*(x-x.^2).^2.*(12*y-6);

