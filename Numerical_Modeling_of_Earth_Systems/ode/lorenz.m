function lorenz % this is only a function to allow function declarations
%
% Lorent'z equation solver 
%
%
% the ... parts will have to be filled in by you 
%
%
% values to solve for 
%
% y(1) : W
% y(2) : T1
% y(3) : T2


% parameters for the equations
parameters.r = ...; % Rayleigh number
parameters.Pr = ..; % Prandtl number 
...


% initial values
y= [...];

time =0;tstop=50;
h = 0.005;% timestep

save_each = 1;
nstep=0;save_step=0;

while(time < tstop)			% loop while time is smaller than tstop
    if(mod(nstep,save_each)==0)		% only save every save_each step
        save_step=save_step+1;
        ysave(save_step,:)=y;
	...
    end
    % advance the y(1:3) solution by one 4th order Runge Kutta step
    y =  y + rkstep(....);  

    nstep=nstep+1;
    time=time+h;
end

figure(1);clf % time series
plot(tsave,ysave(:,2))
xlabel('time');ylabel('temperature');
legend('T_1','T_2')

