% f(x) = x^3 - a = 0
% g(x) = x + C*(x^3 - a)
a=2;
n=10;
g=1.0;
C=-0.21;
     sq(1)=g;
     for i=2:n
      sq(i)= sq(i-1) + C*(sq(i-1)^3 -a);
     end
     hold off
     f=plot([0 n],[a^(1./3.) a^(1./3.)],'b')
     set(f,'LineWidth',2);
     hold on
     f=plot(sq,'r')
     set(f,'LineWidth',2);
     f=plot( (sq-a^(1./3.))/(a^(1./3.)),'g')
     set(f,'LineWidth',2);
     legend('Exact','Iteration','Error');
     f=title(['a = ' num2str(a) ', C = ' num2str(C)])
     set(f,'FontSize',16);
     grid on

 
