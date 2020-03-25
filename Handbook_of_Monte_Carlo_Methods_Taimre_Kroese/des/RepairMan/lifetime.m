%Repairman/lifetime.m
function out = lifetime(i)
global alpha 
out = (-log(rand))^(1/alpha(i));
