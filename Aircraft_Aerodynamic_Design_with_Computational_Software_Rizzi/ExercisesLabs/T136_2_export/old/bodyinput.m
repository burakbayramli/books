function[geo]=bodyinput(geo)
disp(' ')
disp(' This will add blunt body data to the geo struct. ')
disp(' ')
nbod=input('Number of blunt bodies: ');

for i=1:nbod
    
    
    
    disp('________________________________')
    disp(strcat('Data regarding blunt body number :',(num2str(i))));
    data=input('Body length, [m]: ');
    if isinptok(data)==1;
       geo.body.length(i)=data;    
    end
    
    data=input('Body diameter, [m]: ');
    if isinptok(data)==1;
       geo.body.diam(i)=data;    
    end
    
    data=input('Body interference factor: ');
    if isinptok(data)==1;
       geo.body.inter(i)=data;    
    end
    
    
    
    
end