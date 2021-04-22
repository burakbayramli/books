a=2;    %Number for which the sqrt is to be computed
n=10;   %Number of iteration in recursion
g=2;    %Initial guess
% Number of Digits
dig=5;
     sq(1)=g;
     for i=2:n
      sq(i)= 0.5*radd(sq(i-1),a/sq(i-1),dig);
     end
     '      i       value   '
     [ [1:n]' sq']
     hold off
     plot([0 n],[sqrt(a) sqrt(a)],'b')
     hold on
     plot(sq,'r')
     plot(a./sq,'r-.')
     plot((sq-sqrt(a))/sqrt(a),'g')
     legend('sqrt','xn','s/xn','Relative Err')
     grid on

 
