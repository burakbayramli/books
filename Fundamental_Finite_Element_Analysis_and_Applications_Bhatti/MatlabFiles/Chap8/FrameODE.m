function ddot = FrameODE(t, d)
% ddot = FrameODE(t, d)
% function to set up equations for a transient frame problem
global Mf Kf Rf
if t <= 0.6
    ft = 50*t + 20;
elseif t <= .85
        ft = 20;
    elseif t <= 1.2
            ft = 190 - 200*t;
        elseif t <= 1.4
                ft = 250*t - 350;
            else
                ft = 0;
            end
        end
    end
end
n=length(d);
u = d(1:n/2);
v = d(n/2+1:n); 
vdot = inv(Mf)*(Rf*ft - Kf*u);
udot = v;
% format short g
% soln=[t, ft, u(2), udot(2), vdot(2)]
ddot = [udot; vdot];