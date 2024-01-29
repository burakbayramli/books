
function [gauss_points, gauss_weights]=GetQuadEdgeGauss8x1()
  
  gauss_points=zeros(8,2);
  gauss_weights=zeros(8,1);
  
  %---1st point
  gauss_points(1,1)=1.0/sqrt(3);
  gauss_points(1,2)=1.0;
  gauss_weights(1)=1.0;
  
  %---2nd point  
  gauss_points(2,1)=-1.0/sqrt(3);
  gauss_points(2,2)=1.0;
  gauss_weights(2)=1.0;
  
  %---3rd point  
  gauss_points(3,1)=-1.0;
  gauss_points(3,2)=1.0/sqrt(3);
  gauss_weights(3)=1.0;
 
  %---4th point  
  gauss_points(4,1)=-1;
  gauss_points(4,2)=-1.0/sqrt(3);
  gauss_weights(4)=1.0;
        
  %---5th point
  gauss_points(5,1)=-1.0/sqrt(3);
  gauss_points(5,2)=-1.0;
  gauss_weights(5)=1.0;
  
  %---6th point  
  gauss_points(6,1)=1.0/sqrt(3);
  gauss_points(6,2)=-1.0;
  gauss_weights(6)=1.0;
  
  %---7th point  
  gauss_points(7,1)=1.0;
  gauss_points(7,2)=-1.0/sqrt(3);
  gauss_weights(7)=1.0;
  
  %---8th point  
  gauss_points(8,1)=1.0;
  gauss_points(8,2)=1.0/sqrt(3);
  gauss_weights(8)=1.0;
  
  