% EMD and Hilbert Spectrum example
% "boink" signal

%read data file
[yin,fin]=wavread('boink.wav'); %read wav file

y=yin(1:2:6000)'; %select a signal segment and decimate by 1/2
fs=fin/2;
sy=y; %for sound
Ny=length(y);


% EMD decomposition ----------------------------------
nim=5; %number of imfs to be found
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

% Prepare the Hilbert spectrum image
Fq=zeros(nim,Ny); %frequencies
Am=zeros(nim,Ny); %amplitudes
kk=1/(2*pi);
for ni=1:nim,
  X=hilbert(Mimf(ni,:));
  Am(ni,:)=abs(X);
  Ph=atan2(imag(X),real(X));
  Fq(ni,2:end)=kk*diff(Ph); %frequencies
end

%build a picture, selecting some imfs
kk=floor(Ny/2);
Phht=zeros(kk,Ny);
% find TF points and associate Amplitude
for nn=1:Ny,
    for ni=1:3, %choose imf(1),(2) and (3)
        aux=1+floor(Ny*abs(Fq(ni,nn))); %find a freq. point
        Phht(aux,nn)=Am(ni,nn);
    end;
end;
        
% display-------------------------------------

figure(1)
for jj=1:nim,
subplot(nim,1,jj)
plot(Mimf(jj,:),'k');
end

disp('please wait for second figure')
figure(2)
L=1400; %number of selected image lines
q=2*L/Ny; %corresponding max freq.
tiv=1/fs; fiv=fs/Ny;
t=0:tiv:(Ny-1)*tiv;
f=0:fiv:(q*fs/2)-fiv;
colormap('gray');
AA=Phht(1:L,:)>0.08; %visualization threshold
contourf(t,f,1-AA);
title('Hilbert Spectrum');
xlabel('Time'); ylabel('Hz');

soundsc(sy,fs)
