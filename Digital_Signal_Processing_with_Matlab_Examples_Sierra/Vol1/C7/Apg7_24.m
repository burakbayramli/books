% EMD example
% ECG signal

%read data file
fs=200;
fer=0;
while fer==0,
    fid2=fopen('ECGa.txt','r');
    if fid2==-1, disp('read error')
    else ecgdat=fscanf(fid2,'%f \r\n'); fer=1;
    end;
end;
fclose('all');

y=ecgdat(1:1400)'; %select a signal segment
Ny=length(y);

% EMD decomposition ----------------------------------
nim=10; %number of imfs to be found
Mimf=zeros(nim,Ny);

for nn=1:nim,
    h=y; %initial signal
    StD=1; %standard deviation (used for stop criterion)
   
    while StD>0.3,      

        % find max/min points
        D=diff(h); %derivative
        popt=[]; %to store max or min points
        for i=1:Ny-2,
            if D(i)==0,
                popt=[popt,i];
            elseif sign(D(i))~=sign(D(i+1));
                popt=[popt,i+1]; %the zero was between i and i+1
            end;
        end;

        if size(popt,2) <2 %got a final residue
            break
        end;

        %distinguish maxima and minima
        No=length(popt);
        % if first one is a maximum
        if popt(1)>popt(2),
            pmax=popt(1:2:No);
            pmin=popt(2:2:No);
        else
            pmax=popt(2:2:No);
            pmin=popt(1:2:No);
        end;

        %force endpoints
        pmax=[1 pmax Ny];
        pmin=[1 pmin Ny];

        %create envelopes using spline interpolation
        maxenvp=spline(pmax,h(pmax),1:Ny);
        minenvp=spline(pmin,h(pmin),1:Ny);

        %mean of envelopes
        m = (maxenvp+minenvp)/2;
        oldh=h;
        h=h-m; %subtract mean to h
        
        %compute StD
        ipsi=0.0000001;
        StD=sum(((oldh-h).^2)./(oldh.^2+ipsi));    
    end
    
    Mimf(nn,:)=h; %store IMF(nn)
    
    y=y-h; %subtract the IMF from the signal

end
        
% display-------------------------------------

figure(1)
for jj=1:5,
subplot(5,1,jj)
plot(Mimf(jj,:),'k');
end

figure(2)
for jj=6:10,
subplot(5,1,jj-5)
plot(Mimf(jj,:),'k');
end

figure(3)
plot(ecgdat(1:1400),'k');
title('heartbeat signal')
axis([1 1400 6 9.5]);
