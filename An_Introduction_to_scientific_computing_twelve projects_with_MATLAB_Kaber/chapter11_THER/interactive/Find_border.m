%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%==============================================
% Indetifies the internal boundary of the mesh 
%==============================================

function [xb, yb]=Find_border(Ireft)

global XYs I123 Reft 

ref1=Ireft(1);
vt1=find(Reft==ref1);It1=I123(vt1,:);
vt2=find(Reft~=ref1);It2=I123(vt2,:);

xb=[];yb=[];
for i=1:size(It1,1)
   clear i1 j1 i2 j2 i3 j3
   [i1, j1]=find(It2 == It1(i,1));
   [i2, j2]=find(It2 == It1(i,2));
   [i3, j3]=find(It2 == It1(i,3));
   
   if length(i1) >0  & length(i2) >0
      for k=1:length(i1)
      	clear vc; vc=find(i2 == i1(k));
      	if length(vc) > 0
            xb=[xb;XYs(It1(i,1:2)',1)];
            yb=[yb;XYs(It1(i,1:2)',2)];
            %plot(XYs(It1(i,1:2)',1),XYs(It1(i,1:2)',2));hold on;
         end
      end
   end
   
   if length(i1) >0  & length(i3) >0
      for k=1:length(i1)      
      	clear vc; vc=find(i3 == i1(k));
         if length(vc) > 0
            xb=[xb;XYs(It1(i,1:2:3)',1)];
            yb=[yb;XYs(It1(i,1:2:3)',2)];
         	%plot(XYs(It1(i,1:2:3)',1),XYs(It1(i,1:2:3)',2));hold on;
      	end
   	end
	end
   
   if length(i2) >0 & length(i3) > 0
     	for k=1:length(i2)
      	clear vc; vc=find(i3 == i2(k));
         if length(vc) > 0
            xb=[xb;XYs(It1(i,2:3)',1)];
            yb=[yb;XYs(It1(i,2:3)',2)];
         	%plot(XYs(It1(i,2:3)',1),XYs(It1(i,2:3)',2));hold on;
      	end
      end
	end
 
end

       
