% using_Sx.m
clear all,clc
global  GRAPH % create global object GRAPH

  E=[1,1,1,2,2,3,4,5,6,7,7,7,8,9,10,11,12,13;
     2,3,4,5,6,7,7,8,8,9,10,11,9,13,14,12,14,14]';

GRAPH.E=E; % create structure field name 'E' to store matrix E
x=rand(1,18); % generate some random times
Sx=S(x) % call function S(x)
