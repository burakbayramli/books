% Using Meyer windows to build a curvelet

L=170; %grid side length
X=zeros(L+1,1); Y=zeros(L+1,1);
CV=zeros(L+1,L+1); %space for curvelet

for nl=0:L,
   Y(nl+1)=nl/100; yaux=nl/100;
   
   for nc=0:L,
      X(nc+1)=nc/100; xaux=nc/100;
      
      r=sqrt(xaux^2+yaux^2);
      
      if xaux>0,
         theta=atan(yaux/xaux);
      else
         theta=pi/2;
      end;   
      
      V=0;
      if theta<=(1/3), 
         V=1; 
      else
         if theta<=(2/3),
            x=(3*theta)-1;
            nu=0;
            if x>0,
               if x<1,
                  s1=exp(-((1/(1+x)^2)+(1/(1-x)^2)));
                  s2=exp(-((1/(1+(x-1))^2)+(1/(1-(x-1))^2)));
                  nu=s2/(s2+s1);
               end;
            end;
            if x>=1, nu=1; end;
         
            V=cos((pi*nu)/2);
         end;
      end;
            
      W=0;
      if r>=(2/3),
         if r<=(5/6),
            x=5-(6*r);
            nu=0;
            if x>0,
               if x<1,
                  s1=exp(-((1/(1+x)^2)+(1/(1-x)^2)));
                  s2=exp(-((1/(1+(x-1))^2)+(1/(1-(x-1))^2)));
                  nu=s2/(s2+s1);
               end;
            end;
            if x>=1, nu=1; end;
         

            W=cos((pi*nu)/2);
         else
            if r<=(4/3),
               W=1;
            else
               if r<=(5/3),                  
 						x=3-(4*r);
                  nu=0;
                  if x>0,
                    if x<1,
                       s1=exp(-((1/(1+x)^2)+(1/(1-x)^2)));
                       s2=exp(-((1/(1+(x-1))^2)+(1/(1-(x-1))^2)));
                       nu=s2/(s2+s1);
                    end;
                 end;
                 if x>=1, nu=1; end;
         
                  W=cos((pi*nu)/2);
               end;
            end;
         end;
       end;      
            
      CV(nl+1,nc+1)=W*V;
      
   end;   
end; 

Y=[flipud(Y);-Y]; %use symmetry around horizontal
Z=zeros(L+1,L+1);
CV=[flipud(CV);CV];


figure(1)
colormap('jet');
mesh(X,Y,CV);
view(20, 60);
title('Basic curvelet in the frequency plane: 3D view');

figure(2)
colormap('winter');
contourf(X,Y,CV,[0.1 0.3 0.5 0.7 0.9]);
view(2);
title('Basic curvelet in the frequency plane: support');
