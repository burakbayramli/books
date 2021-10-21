
% linear traffic flow with discontinuous speed limit

global umax umax1

   exno = 5;
   umax = 2.0;
   umax1 = 1.0;
   speed = 'speedlinear';
   wall = 100;
   xlim = [-45 45];
   tlim = [-2 25];
   X = [-81:3:0 10:10:50] + .001; 
   nsteps = 1;
   tend = 20;


maketrafficfig;

