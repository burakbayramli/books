%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
close all
coll=[0,1]'; 
x=linspace(0,1,100)';
X=[];Y=[];
for alpha=0:5:20
   T=[coll [0;1]];
   T=[T(:,1) alpha*ones(size(coll)), T(:,2)];
   for k=1:alpha
      T=[T zeros(size(coll))];
   end;
   [xx,dd]=APP_ddHermite(T);n=length(dd);
   y=dd(n)*ones(size(x));
   for k=n-1:-1:1
       y=dd(k)+y.*(x-xx(k));
   end;
   X=[X x];Y=[Y y];
end;
plot(X,Y,'LineWidth',2,'MarkerSize',10)
set(gca,'XTick',0:.2:1,'FontSize',24)
%set(gca,'YTick',0:.2:1)
axis([0 1 0 1.1])
title('Hermite Interpolation');
text(.7,.7,'\leftarrow m=0','FontSize',24)
  text(.4,.95,'m=20 \downarrow','HorizontalAlignment','left','FontSize',24)
  text(.68,.9,'\leftarrow m=5','FontSize',24)
