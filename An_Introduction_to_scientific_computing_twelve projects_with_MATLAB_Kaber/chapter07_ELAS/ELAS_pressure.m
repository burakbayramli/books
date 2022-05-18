%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      function p=ELAS_pressure(x,y,u); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    function p=ELAS_pressure(x,y,u)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Definition of the total pressure at point M(x,y) 
%%   in order to solve exercise 2 of project 7
%%   Solution of the microphone problem (non-linear equation)
%%   ELAS: elastic deformation of a thin plate
%%
%%
%%   Input  : x, y coordinates of point M
%%               u deformation in this point 
%%
%%   Output : p total pressure = acoustic + electrostatic
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%     acoustic pressure
      Pa=1.0; % unit  Pascal
%     physical values : from 1.e-03 to 10.
%     electrostatic pressure
%     capacitor thickness
      h=5.e-06; % 5 microns
%     surface of plate
%     S=1.e-09; % 1 mm x 1 mm
%     air permittivity
      eps=8.85e-12; % unit  Faraday/m
%     polarization potential
      V=25.; %  unit  Volt
      hu=h-u;
      Pe=eps*V*V/(2*hu*hu); 
%     total pressure
      p=Pa+Pe;
