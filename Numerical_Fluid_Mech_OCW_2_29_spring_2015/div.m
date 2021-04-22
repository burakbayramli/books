a=10;
n=10;
g=0.19;
% x=1/a
% f = ax - 1 = 0
% f'=a
% f/f' = x(ax-1)
% x_n+1 =x_n - x_n( a*x_n -1)
     sq(1)=g;
     for i=2:n
      sq(i)=sq(i-1) - sq(i-1)*(a*sq(i-1) -1) ;
     end
     hold off
     f=plot([0 n],[1/a 1/a],'b')
     set(f,'LineWidth',2);
     hold on
     f=plot(sq,'r')
     set(f,'LineWidth',2);
     f=plot((sq-1/a)*a,'g')
     set(f,'LineWidth',2);
     grid on
     f=legend('Exact','Iteration','Rel. Error');
     set(f,'FontSize',14)
     f=title(['x = 1/' num2str(a)])
     set(f,'FontSize',16)