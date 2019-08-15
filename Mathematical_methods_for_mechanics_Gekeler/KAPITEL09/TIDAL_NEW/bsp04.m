function [p,e,t,e2] = bsp04g
% channel  cf. Petera/Nassehi
% Int. J. Num. Meth. Eng. 39(1996), 4159-4182
%
XSCALE =1;        % XBAI  : scale factor in X axis
YSCALE =1;        % YBAI  : scale factor in Y axis
%
p = [0, 0, 0, 1, 1, 1;
     2, 1, 0, 2, 1, 0];
ta = [3, 5, 5, 1; 6, 2, 4, 2; 5, 3, 1, 5]; t = ta;
e1a = [3;6]; e1 = e1a;
e2 = [6, 5;5, 4];
e3a = [4;1]; e3 = e3a;
e4 = [1, 2; 2, 3];

for I = 1:39
    p = [p, [I+1, I+1, I+1; 2, 1, 0]];
    t = [t, ta + 3*I];
    e1 = [e1, e1a + 3*I];
    e2 = e2 + 3;
    e3 = [e3a + 3*I, e3];
end
LE = size(e1,2); e1 = [e1;zeros(2,LE);ones(1,LE)];
LE = size(e2,2); e2 = [e2;zeros(2,LE);2*ones(1,LE)];
LE = size(e3,2); e3 = [e3;zeros(2,LE);3*ones(1,LE)];
LE = size(e4,2); e4 = [e4;zeros(2,LE);4*ones(1,LE)];

e = [e1, e2, e3, e4];

p = [XSCALE*p(1,:);YSCALE*p(2,:)];

waterdepth = zeros(1,size(p,2)); % cf. initial condition
p = [p;waterdepth];
