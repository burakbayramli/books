%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [XQ1,YQ1,td1,tf1,XQ2,YQ2,td2,tf2,iter]=CAGD_dbezier(XP1,YP1,d1,f1,XP2,YP2,d2,f2,iter);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [XQ1,YQ1,td1,tf1,XQ2,YQ2,td2,tf2,iter]
%%   = CAGD_dbezier(XP1,YP1,d1,f1,XP2,YP2,d2,f2,iter)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Tracking the intersection point of two Bézier curves
%%   
%%   Input : T sampling values 
%%           XP1, YP1 control points coordinates 
%%           XP2, YP2 control points coordinates 
%%           d1,f1  control points parameters (beginning-end)
%%           d2,f2  control points parameters (beginning-end)
%%           iter   number of this procedure calls
%%
%%   Ouput : X, Y coordinates of P(T) in R^2
%%           XQ1, YQ1 new control points coordinates 
%%           XQ2, YQ2 new control points coordinates 
%%           td1,tf1  new control points parameters  
%%           td2,tf2  new control points parameters  
%%           iter = iter + 1 
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
   [test,xi,xa,yi,ya]=CAGD_rbezier(XP1,YP1,XP2,YP2);
   XQ1=XP1;YQ1=YP1;XQ2=XP2;YQ2=YP2;
   td1=d1;tf1=f1;td2=d2;tf2=f2;
%%
%%  Check convergence (iterations number) 
%%
%  iterations count
   iter=iter+1;
   itmax=20;
   if ( iter > itmax )
      fprintf('\n Recursivity stop : non converging scheme ');
      trectan(xi,xa,yi,ya,'b'); 
      return
   end
%%
%%  Check convergence (convexity measure)
%%
   h1=CAGD_convex(XP1,YP1);h2=CAGD_convex(XP2,YP2);h=max(h1,h2);
   seuilc=5.e-05;
   if ( h < seuilc ) 
      fprintf('\n Convergence for convexity h = %f ',h);
      fprintf('\n Iteration %d ',iter);
      return
   end
%%
%%  Convergence in abscissa ?
%%
   seuil=1.e-04;
   if ( abs(xa-xi) < seuil )
      fprintf('\n Convergence in x');
      fprintf('\n Iteration %d ',iter);
      return
   end
%%
%%  Convergence in ordinate ?
%%
   if ( abs(ya-yi) < seuil ) 
      fprintf('\n Convergence in y');
      fprintf('\n Iteration %d ',iter);
      return
   end
%%
%% Splitting the curves
%%
   char=int2str(iter);char=strcat('RI: ',char);
%  paramter t
   tm1=(d1+f1)*0.5;
   [XP1a,YP1a]=CAGD_cast1(tm1,XP1,YP1);
   [XP1b,YP1b]=CAGD_cast2(tm1,XP1,YP1);
   tm2=(d2+f2)*0.5;
   [XP2a,YP2a]=CAGD_cast1(tm2,XP2,YP2);
   [XP2b,YP2b]=CAGD_cast2(tm2,XP2,YP2); 
%% intersection tests
   [test1,xmi1,xma1,ymi1,yma1]=CAGD_rbezier(XP1a,YP1a,XP2a,YP2a);
   [test2,xmi2,xma2,ymi2,yma2]=CAGD_rbezier(XP1b,YP1b,XP2a,YP2a);
   [test3,xmi3,xma3,ymi3,yma3]=CAGD_rbezier(XP1a,YP1a,XP2b,YP2b);
   [test4,xmi4,xma4,ymi4,yma4]=CAGD_rbezier(XP1b,YP1b,XP2b,YP2b);
   if (test1==2) 
      CAGD_trectan(xmi1,xma1,ymi1,yma1,'k');char1=strcat(char,'-1');text(xma1*1.05,ymi1*1.05,char1);
%%      fprintf('\n Intersection 1 - 1');   
%%      fprintf('\n xmi xma ymi yma %8.5f %8.5f %8.5f %8.5f ',xmi1,xma1,ymi1,yma1);
      [XQ1,YQ1,td1,tf1,XQ2,YQ2,td2,tf2,iter]=CAGD_dbezier(XP1a,YP1a,d1,tm1,XP2a,YP2a,d2,tm2,iter);
   elseif (test2==2) 
      CAGD_trectan(xmi2,xma2,ymi2,yma2,'k');char2=strcat(char,'-2');text(xma2*1.05,ymi2*1.05,char2)
%%      fprintf('\n Intersection 2 - 1');
%%      fprintf('\n xmi xma ymi yma %8.5f %8.5f %8.5f %8.5f ',xmi2,xma2,ymi2,yma2);
      [XQ1,YQ1,td1,tf1,XQ2,YQ2,td2,tf2,iter]=CAGD_dbezier(XP1b,YP1b,tm1,f1,XP2a,YP2a,d2,tm2,iter);
   elseif (test3==2) 
      CAGD_trectan(xmi3,xma3,ymi3,yma3,'k');char3=strcat(char,'-3');text(xma3*1.05,ymi3*1.05,char3);
%%      fprintf('\n Intersection 1 - 2');
%%      fprintf('\n xmi xma ymi yma %8.5f %8.5f %8.5f %8.5f ',xmi3,xma3,ymi3,yma3);
      [XQ1,YQ1,td1,tf1,XQ2,YQ2,td2,tf2,iter]=CAGD_dbezier(XP1a,YP1a,d1,tm1,XP2b,YP2b,tm2,f2,iter);
   elseif (test4==2) 
      CAGD_trectan(xmi4,xma4,ymi4,yma4,'k');char4=strcat(char,'-4');text(xma4*1.05,ymi4*1.05,char4);
%%      fprintf('\n Intersection 2 - 2');
%%      fprintf('\n xmi xma ymi yma %8.5f %8.5f %8.5f %8.5f ',xmi4,xma4,ymi4,yma4);
      [XQ1,YQ1,td1,tf1,XQ2,YQ2,td2,tf2,iter]=CAGD_dbezier(XP1b,YP1b,tm1,f1,XP2b,YP2b,tm2,f2,iter);
   end