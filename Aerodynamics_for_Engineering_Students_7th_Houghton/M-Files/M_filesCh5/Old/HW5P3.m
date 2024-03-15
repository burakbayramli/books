%
%  HW5 Problem 3 and 4
clear;clc
% %Problem 3 % Given information (input):
% mu = 1; 
% U = 1; % m/s 
% % SOLUTION:
% x = -1:.02:1; 
% y = -1:.02:1;
% for mm = 1:length(x)
%     for nn = 1:length(y)
%         xx(mm,nn) = x(mm); yy(mm,nn) = y(nn);
%         psi(mm,nn) = U * y(nn) ...
%             - (mu/2/pi) * y(nn)/(x(mm)^2+y(nn)^2);
%     end
% end
% contour(xx,yy,psi,[0 0],'k'),axis image
% hold on
% contour(xx,yy,psi,200)
% Problem 4
% SOLUTION:
m = -10;
c = 3;
x = -4:.02:4; 
y = -4:.02:4;
for mm = 1:length(x)
    for nn = 1:length(y)
        xx(mm,nn) = x(mm); yy(mm,nn) = y(nn);
        psi(mm,nn) = (m/2/pi)*atan2(y(nn),x(mm))...
            - (m/pi)*atan2(y(nn),x(mm) - c);
    end
end
contour(xx,yy,psi,[-5:.1:5]),hold on
contour(xx,yy,psi,[m/2 -m/2],'k'),axis image, axis([-4 4 -4 4])

