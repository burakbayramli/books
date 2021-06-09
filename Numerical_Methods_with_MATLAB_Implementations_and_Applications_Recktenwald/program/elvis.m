% Elvis  Script file to demonstrate output from fprintf
name = 'Elvis';
age = str2num(datestr(now,10))-1935;
fprintf('%s is %d years old\n',name,age);

fprintf('\n\n');
name = 'Elvis';
age = str2num(datestr(now,10))-1935;
fprintf('%9s\n',name);    %  Print name in a field 9 characters wide
fprintf('1234567890\n');  %  Print column indicators
fprintf('%5d\n',age);     %  Print age in a field 5 characters wide

fprintf('\n\n');
height = 161.3;
fprintf('1234567890\n');       %  print column indicators
fprintf('%6.1f\n',height);
fprintf('1234567890\n');       %  print column indicators
fprintf('%8.3f\n',height);
fprintf('1234567890123\n');    %  print column indicators
fprintf('%11.3e\n',height);

fprintf('\n\n');
x = linspace(1,20,5);
fprintf('   k     x(k)\n');
for k=1:length(x)
   fprintf('%4d     %5.2f\n',k,x(k));
end
