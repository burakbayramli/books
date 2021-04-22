a=2;
n=6;
g=2;
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
     grid on
     legend('Exact','sq','a./sq','Error');
