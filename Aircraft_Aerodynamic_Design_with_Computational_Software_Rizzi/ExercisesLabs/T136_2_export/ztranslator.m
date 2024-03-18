%Matlabscript to generate Python memory structs with the 

[a b c]=size(lattice.VORTEX);
V=lattice.VORTEX;

STR=[];
STR=[STR '['];
for i=1:a
    
    STR=[STR '['];
    for j=1:b
        
        STR=[STR '['];
        for k=1:c
            
            STR=strcat(STR,num2str(V(i,j,k)));
            if k<(c)
                STR=[STR ','];
                STR=[STR 32];
            end            
        end
        STR=[STR ']'];
        if j<b
            STR=[STR ',' '\n'];
        end
    end
    STR=[STR ']' ];
    if i<a
        STR=[STR ',' '\n'];
    end
end
STR=[STR ']'];

V=sprintf(STR)

%% Panels
P=lattice.XYZ;
b=5;
STR=[];
STR=[STR '['];
for i=1:a
    
    STR=[STR '['];
    for j=1:b
        
        STR=[STR '['];
        for k=1:c
            
            STR=strcat(STR,num2str(P(i,j,k)));
            if k<(c)
                STR=[STR ','];
                STR=[STR 32];
            end            
        end
        STR=[STR ']'];
        if j<b
            STR=[STR ',' '\n'];
        end
    end
    STR=[STR ']' ];
    if i<a
        STR=[STR ',' '\n'];
    end
end
STR=[STR ']'];
P=sprintf(STR)


