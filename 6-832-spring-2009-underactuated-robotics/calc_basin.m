% Simulates a simple pendulum and calculates the basin of
% attraction for a given fixed point.
function calc_basin
% pendulum parameters
m=1; g = 9.8;
l = 1; I = m*l*l;
dt = 1e-2;
b = 0;
u = 0;

th_bins = linspace(-2*pi,2*pi,50);
thdot_bins = linspace(-2*g/l,2*g/l,50);
[th,thdot] = ndgrid(th_bins,thdot_bins);
basin = zeros(size(th));
max_iterations = 1e4;

fp = [0 0]'; % this is the fixed point
for i=1:size(th,1)
    for j=1:size(th,2)
       x =  [th(i,j) thdot(i,j)]';
        % simulate the dynamics until they converge/diverge/or hit
        % max_iterations
       for t=1:max_iterations % <=== FILL IN THIS LOOP 
            % run the dynamics...
            
            % check if the state has converged...
            
            % check if the state has diverged...
       end
    end
    if (mod(i,5)==0)
        disp([num2str((i/size(th,1))*1e2),' % done']);
    end
end

imagesc([th(1,1) th(end,1)],[thdot(1,1) thdot(1,end)],basin');
axis xy; colormap('winter');colorbar;
xlabel('theta'); ylabel('theta dot');

    function xdot = dynamics(x,u)
        xdot = [x(2); (u-m*g*l*sin(x(1))-b*x(2))./I];
    end
end