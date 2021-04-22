function ddot = HeatODE(t, d)
% ddot = HeatODE(t, d)
% function to set up equations for a transient heat flow problem
global Mf Kf Rf
ddot = inv(Mf)*(Rf - Kf*d);