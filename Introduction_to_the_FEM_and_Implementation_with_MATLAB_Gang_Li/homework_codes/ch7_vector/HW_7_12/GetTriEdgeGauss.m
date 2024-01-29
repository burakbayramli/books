function [gauss_points, gauss_weights]=GetTriEdgeGauss()
% current: 2 points per edge, can be expanded
gauss_points=zeros(6,2);
gauss_weights=zeros(6,1);
p1=0.5-1/sqrt(3)*0.5;
p2=0.5+1/sqrt(3)*0.5;
% 1st point
gauss_points(1,1)=p1;
gauss_points(1,2)=0.0;
gauss_weights(1)=0.5;
% 2nd point  
gauss_points(2,1)=p2;
gauss_points(2,2)=0.0;
gauss_weights(2)=0.5;
% 3rd point  
gauss_points(3,1)=p2;
gauss_points(3,2)=p1;
gauss_weights(3)=0.5*sqrt(2);
% 4th point  
gauss_points(4,1)=p1;
gauss_points(4,2)=p2;
gauss_weights(4)=0.5*sqrt(2);
% 5th point
gauss_points(5,1)=0.0;
gauss_points(5,2)=p1;
gauss_weights(5)=0.5;
% 6th point  
gauss_points(6,1)=0.0;
gauss_points(6,2)=p2;
gauss_weights(6)=0.5;