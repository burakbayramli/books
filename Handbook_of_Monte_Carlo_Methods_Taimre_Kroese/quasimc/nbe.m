function na = nbe(a,b)
numd = numel(a);
na = a;
carry = true;
for i=numd:-1:1
    if carry
        if a(i) == b-1
            na(i) = 0;
        else
            na(i) = a(i) + 1;
            carry = false;
        end
    end
end
if carry
    na = [1,na];
end
