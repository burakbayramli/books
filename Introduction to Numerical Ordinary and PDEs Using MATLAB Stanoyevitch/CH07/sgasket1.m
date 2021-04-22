function sgasket1(V1,V2,V3,ngen)
%input variables: V1,V2,V3 should be the vertices [0 0], [1,sqrt(3)], 
%and [2,0] of a particular equilateral triangle in the plane taken as 
%row vectors, ngen is the number of iterations to perform in 
%Sierpinski gasket generation. 
%The gasket will be drawn in medium gray color.

%first form matrices for similitudes
   S1=[.5 0 0;0 .5 0;0 0 1];
   S2=[.5 0 1; 0 .5 0;0 0 1];
   S3=[.5 0 .5; 0 .5 sqrt(3)/2;0 0 1];
   
if ngen == 0
   %Fill triangle
   fill([V1(1) V2(1) V3(1) V1(1)], [V1(2) V2(2) V3(2) V1(2)], [.5 .5 .5])
   hold on
else %recursivly invoke the same function on three outer subtriangles
   
   %form homogenous coordinate matrices for three vertices of triangle
   A=[V1; V2; V3]';, A(3,:)=[1 1 1];
   %next apply the similitudes to this matrix of coordinates
   A1=S1*A;, A2=S2*A;, A3=S3*A;
   %finally, reapply sgasket to the corresponding three triangles with ngen bumped 
   %down by 1.  Note, vertex vectors have to be made into row vectors using ' (transpose).  
   sgasket1(A1([1 2],1)', A1([1 2],2)', A1([1 2],3)', ngen-1)
   sgasket1(A2([1 2],1)', A2([1 2],2)', A2([1 2],3)', ngen-1)
   sgasket1(A3([1 2],1)', A3([1 2],2)', A3([1 2],3)', ngen-1)
end


