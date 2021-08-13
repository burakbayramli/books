function ppbouss_pthistory(ttch,uxh,uyh,dph,Th,nperiod,itp);
%PPBOUSS_PTHISTORY postprocess periodic point data 
% ppbouss_pthistory(ttch,uxh,uyh,dph,Th,nperiod,0);
%   input
%       ttch             time step history
%       uxh,uyh,dph,Th   point data history
%       nperiod          number of periods to average over
%       itp              1/0 refined/simple interpolation switch
%   IFISS function: MLM; DJS; 3 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic
np=nperiod;
dt(1:7)=0.0;   %   periods of oscillations
n=size(ttch,1);
dpth(1:n,1:7)=0.0; 
dpth(:,1)=ttch; dpth(:,2)=uxh; dpth(:,3)=uyh;
dpth(:,4)=dph; dpth(:,5)=Th; 
%
for j=2:5   %   loop over all types of dofs
   xmin(1:np)=0.0;    %   min values
   xmax(1:np)=0.0;    %   max values
   tmin(1:np)=0.0;    %   time of min values
   tmax(1:np)=0.0;    %   time of max values
   k=size(dpth,1); l=np;
   while(l>0)    %   loop over np extremal values
      if(dpth(k,j)>=dpth(k-1,j))   %   signal ascending
         while(dpth(k,j)>=dpth(k-1,j))  %   finding minimum
            k=k-1;
         end
         if(itp==1)   %  Lagrange quadratic interpolation
            %   local quadratic interpolant nodes 
            xc=0.0; xp=dpth(k+1,1)-dpth(k,1); xn=dpth(k-1,1)-dpth(k,1);
            %   function values at interpolation nodes
            fc=dpth(k,j); fp=dpth(k+1,j); fn=dpth(k-1,j);
            ffc=fc/((xc-xp)*(xc-xn));
            ffp=fp/((xp-xn)*(xp-xc));
            ffn=fn/((xn-xc)*(xn-xp));
            %   local quadratic interpolant coefficients
            a=ffn+ffc+ffp;                      
            b=ffn*(xc+xp)+ffc*(xn+xp)+ffp*(xn+xc);
            c=ffn*xc*xp+ffc*xn*xp+ffp*xn*xc;
            %   local minimum of the interpolant
            tm=b/(2*a);                   %   local extremum
            tmin(l)=tm+dpth(k,1);         %   time of l-th minimum
            %   value of the local minimum
            xmin(l)=a*tm^2-b*tm+c;        %   l-th minimum
         else   %   simple detection
            xmin(l)=dpth(k,j);   %   l-th minimum
            tmin(l)=dpth(k,1);   %   time of l-th minimum
         end   %   if 
         while(dpth(k,j)<=dpth(k-1,j))   %   finding maximum
            k=k-1;
         end
         if(itp==1)   %  Lagrange quadratic interpolation
            %   local quadratic interpolant nodes  
            xc=0.0; xp=dpth(k+1,1)-dpth(k,1); xn=dpth(k-1,1)-dpth(k,1);
            %   function values at interpolation nodes
            fc=dpth(k,j); fp=dpth(k+1,j); fn=dpth(k-1,j);
            ffc=fc/((xc-xp)*(xc-xn));
            ffp=fp/((xp-xn)*(xp-xc));
            ffn=fn/((xn-xc)*(xn-xp));
            %   local quadratic interpolant coefficients
            a=ffn+ffc+ffp;
            b=ffn*(xc+xp)+ffc*(xn+xp)+ffp*(xn+xc);
            c=ffn*xc*xp+ffc*xn*xp+ffp*xn*xc;
            %   local maximum of the interpolant
            tm=b/(2*a);                  %   local extremum
            tmax(l)=tm+dpth(k,1);        %   time of l-th maximum
            %   value of the local maximum
            xmax(l)=a*tm^2-b*tm+c;       %   l-th maximum
         else   %   simple detection
            xmax(l)=dpth(k,j);   %   l-th maximum
            tmax(l)=dpth(k,1);   %   time of l-th maximum
         end   %   if   
         l=l-1;
      else   %   signal descending
         while(dpth(k,j)<=dpth(k-1,j))   %   finding maximum
            k=k-1;
         end
         if(itp==1)   %  Lagrange quadratic interpolation
            %   local quadratic interpolant nodes  
            xc=0.0; xp=dpth(k+1,1)-dpth(k,1); xn=dpth(k-1,1)-dpth(k,1);
            %   function values at interpolation nodes
            fc=dpth(k,j); fp=dpth(k+1,j); fn=dpth(k-1,j);
            ffc=fc/((xc-xp)*(xc-xn));
            ffp=fp/((xp-xn)*(xp-xc));
            ffn=fn/((xn-xc)*(xn-xp));
            %   local quadratic interpolant coefficients
            a=ffn+ffc+ffp;
            b=ffn*(xc+xp)+ffc*(xn+xp)+ffp*(xn+xc);
            c=ffn*xc*xp+ffc*xn*xp+ffp*xn*xc;
            %   local maximum of the interpolant
            tm=b/(2*a);                  %   local extremum
            tmax(l)=tm+dpth(k,1);        %   time of l-th maximum
            %   value of the local maximum
            xmax(l)=a*tm^2-b*tm+c;   %   l-th maximum
         else   %   simple detection
            xmax(l)=dpth(k,j);   %   l-th maximum
            tmax(l)=dpth(k,1);   %   time of l-th maximum
         end   %   if   
         while(dpth(k,j)>=dpth(k-1,j))   %   finding minimum
            k=k-1;
         end
         if(itp==1)   %  Lagrange quadratic interpolation
            %   local quadratic interpolant nodes  
            xc=0.0; xp=dpth(k+1,1)-dpth(k,1); xn=dpth(k-1,1)-dpth(k,1);
            %   function values at interpolation nodes
            fc=dpth(k,j); fp=dpth(k+1,j); fn=dpth(k-1,j);
            ffc=fc/((xc-xp)*(xc-xn));
            ffp=fp/((xp-xn)*(xp-xc));
            ffn=fn/((xn-xc)*(xn-xp));
            %   local quadratic interpolant coefficients
            a=ffn+ffc+ffp;
            b=ffn*(xc+xp)+ffc*(xn+xp)+ffp*(xn+xc);
            c=ffn*xc*xp+ffc*xn*xp+ffp*xn*xc;
            %   local minimum of the interpolant
            tm=b/(2*a);                  %   local extremum
            tmin(l)=tm+dpth(k,1);        %   time of l-th minimum
            %   value of the local maximum
            xmin(l)=a*tm^2-b*tm+c;   %   l-th minimum           
         else   %   simple detection
            xmin(l)=dpth(k,j);   %   l-th minimum
            tmin(l)=dpth(k,1);   %   time of l-th minimum
         end   
         l=l-1;
      end
   end   %   l while loop
   xmin_av=sum(xmin)/np;
   xmax_av=sum(xmax)/np;
   x_av=0.5*(xmin_av+xmax_av);
   dx=xmax_av-xmin_av;
   dt_max=(tmax(np)-tmax(1))/(np-1);
   dt_min=(tmin(np)-tmin(1))/(np-1);
   dt(j)=0.5*(dt_min+dt_max);
   switch(j)   %   types of dof
       case(2)   %   u_x at P1
          fprintf('Velocity x-component at P1\n');
          fprintf('   ux_min=%12.4f\n',xmin_av);
          fprintf('   ux_max=%12.4f\n',xmax_av);
          fprintf('   d(ux)=ux_max-ux_min=%12.4f\n',dx);
          fprintf('   ux_av=0.5*(ux_max+ux_min)=%12.4f\n',x_av);
          fprintf('   dt(ux)=%12.4f\n',dt(j));
       case(3)   %   u_y at P1
          fprintf('Velocity y-component at P1\n');
          fprintf('   uy_min=%12.4f\n',xmin_av);
          fprintf('   uy_max=%12.4f\n',xmax_av);
          fprintf('   d(uy)=uy_max-uy_min=%12.4f\n',dx);
          fprintf('   uy_av=0.5*(uy_max+uy_min)=%12.4f\n',x_av);
          fprintf('   dt(uy)=%12.4f\n',dt(j)); 
       case(4)   %   dp between P1 and P2
          fprintf('Pressure difference between P1 and P2\n');
          fprintf('   dp_min=%12.4f\n',xmin_av);
          fprintf('   dp_max=%12.4f\n',xmax_av);
          fprintf('   d(dp)=dp_max-dp_min=%12.4f\n',dx);
          fprintf('   dp_av=0.5*(dp_max+dp_min)=%12.4f\n',x_av);
          fprintf('   dt(dp)=%12.4f\n',dt(j)); 
       case(5)   %   T at P1
          fprintf('Temperature at P1\n');
          fprintf('   T_min=%12.4f\n',xmin_av);
          fprintf('   T_max=%12.4f\n',xmax_av);
          fprintf('   dT=T_max-T_min=%12.4f\n',dx);
          fprintf('   T_av=0.5*(T_max+T_min)=%12.4f\n',x_av);
          fprintf('   dt(T)=%12.4f\n',dt(j)); 
   end   %   select
end   %   for loop
dtt=sum(dt)/4;   %   average oscillations period
fprintf('Average oscillations period dt=%12.4f\n',dtt);
return
