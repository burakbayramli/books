function sout = convertnum(n)
[a b]=size(n);
characters=double(n);
s=zeros(20,1);

for i=1:b
   
    
    
    if characters(i)==48
        s(i)=0;
    elseif characters(i)==49
        s(i)=1;
    elseif characters(i)==50
        s(i)=2;
    elseif characters(i)==51        
        s(i)=3;
    elseif characters(i)==52        
        s(i)=4;
    elseif characters(i)==53        
        s(i)=5;
    elseif characters(i)==54        
        s(i)=6;
    elseif characters(i)==55
        s(i)=7;
    elseif characters(i)==56
        s(i)=8;
    elseif characters(i)==57
        s(i)=9;
    else
        sout=[];
        return       
    end
    
    
end

if b==4
    sout=s(1)*1000+s(2)*100+s(3)*10+s(4);
else
   sout=0;
end
   
end