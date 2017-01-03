function m=fctrl(n)
if n<0
   error('The factorial of negative number ??');
   %fprintf('\n\aThe factorial of negative number ??\n');
   %return
else
   m=1;
   for k=2:n, m=m*k; end
end
%  elseif n<=1, m=1; 
%  else m=n*fctrl(n-1); %가급적 recursive calling을 쓰지 않는 것이 좋다. 
%end
