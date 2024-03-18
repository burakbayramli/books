
%Computes the downwash of a propeller slipstream.
%
% Inputs data.XYZ       propeller slipstream
%        data.gamma     propeller vortex strength


function[DW3]=propwash(data,P)
one_by_four_pi=1/(4*pi);
%one_by_four_pi=1;               %WHAT?!

[a b]=size(P);

[psize vsize void]=size(data.XYZ);
%disp('running right')

lemma=ones(1,psize);
LDW=zeros(psize,psize,3);

for i=1:(a)                                     %For each collocationpoint
    for k=1:(psize)                             %For each blade on each propeller
        for j=1:(vsize-1)                       %For each vortex element in that spiral
        
             r1=(squeeze(data.XYZ(k,j,:))'-P(i,:));
             r2=(squeeze(data.XYZ(k,j+1,:))'-P(i,:));
    

            dw(j,:)=mega(r1,r2);

        end
        
        
        DW(i,k,:)=data.gamma(k)*sum(dw,1);
    end
    
end
DW2(:,:)=squeeze(sum(DW,2));
DW3=DW2*one_by_four_pi;

end


function[DW]=mega(r1,r2)
%% First part
near=0.1;

F1=cross(r1,r2);
sq_LF1=(sum(F1.^2));
F2=F1./(sq_LF1);

%% Next part
Lr1=sqrt(sum(r1.^2)); 
Lr2=sqrt(sum(r2.^2));
R1=r1./Lr1;
R2=r2./Lr2;
L1=(R2-R1);

%% Third part
R0=(r2-r1);

radial_distance=sqrt((sq_LF1./(sum(R0.^2))));


%% combinging 2 and 3
L2=  R0*L1';
%% Downwash
DW=F2*L2;
if isnan(DW)
    DW=[0 0 0];
end

DW=DW.*(1-(radial_distance<near));



end
    
    
    
    