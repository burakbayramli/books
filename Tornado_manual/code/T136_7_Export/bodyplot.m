function[]=bodyplot(body);
[a b]=size(body.length);

for i=1:b
   [X Y Z]=cylinder(body.diam(i)/2);
   Z=Z.*body.length(i).*(body.aftfuse(i));
   X=X+body.start(i,3); 
   Y=Y+body.start(i,2);
   Z=Z+body.start(i,1);
   
   figure(3)
   hold on
   axis equal
   surf(Z,Y,X,ones(size(X)));
   
   if body.aftfuse(i)<1
      [X Y Z]=cylinder([body.diam(i)/2 0]);
      Z=Z.*body.length(i).*(1-body.aftfuse(i))+body.length(i).*(body.aftfuse(i));
      X(2,:)=X(2,:)+body.diam(i)/2;
      
      
        X=X+body.start(i,3); 
        Y=Y+body.start(i,2);
        Z=Z+body.start(i,1);
        figure(3)
   hold on
   axis equal
   surf(Z,Y,X,ones(size(X)));
       
   end
   
    
end