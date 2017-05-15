% JPEG simulation
%
P=imread('flowers.jpg'); %read image
P=P(1:256,1:256,:); %256x256 size
N=256;
F=im2double(P);
M=dctmtx(8);
     
% Quantization tables
  qmax = 255;
% quality factors for Y components
  QY = ...
       [16 11 10 16 124 140 151 161;
        12 12 14 19 126 158 160 155; 
        14 13 16 24 140 157 169 156; 
        14 17 22 29 151 187 180 162; 
        18 22 37 56 168 109 103 177; 
        24 35 55 64 181 104 113 192; 
        49 64 78 87 103 121 120 101; 
        72 92 95 98 112 100 103 199];
% quality factors for chroma components 
  QC = ...
       [17 18 24 47 99 99 99 99; 
        18 21 26 66 99 99 99 99; 
        24 26 56 99 99 99 99 99; 
        47 66 99 99 99 99 99 99; 
        99 99 99 99 99 99 99 99; 
        99 99 99 99 99 99 99 99; 
        99 99 99 99 99 99 99 99; 
        99 99 99 99 99 99 99 99];
     
% Scale quantization matrices based on quality factor
  qf = 20;
  if qf < 50
     q_scale = floor(5000 / qf);
  else
     q_scale = 200 - 2 * qf;
  end
  QY = round(QY * q_scale / 100);
  QC = round(QC * q_scale / 100);
 
% RGB to YCbCr
  YC = rgb2ycbcr(F);
     
% Down-sample and decimate chroma
  cb = conv2(YC(:,:,2), [1 1; 1 1]) ./ 4.0;
  cr = conv2(YC(:,:,3), [1 1; 1 1]) ./ 4.0;
  CB = cb(2:2:size(cb, 1), 2:2:size(cb, 2));
  CR = cr(2:2:size(cr, 1), 2:2:size(cr, 2));
  Y = YC(:,:,1);
  L=N/2; %both CB and CR are LxL
     
% 2D DCT, and scaling
  DY = blkproc(Y,[8 8],'P1*x*P2',M,M' ) .* qmax;
  DCB = blkproc(CB,[8 8], 'P1*x*P2',M,M') .* qmax;
  DCR = blkproc(CR,[8 8], 'P1*x*P2',M,M') .* qmax;
     
% Quantize DCT coefficients
  qDY = blkproc(DY,[8 8],'round(round(x)./P1)',QY);
  qDCB = blkproc(DCB,[8 8],'round(round(x)./P1)',QC);
  qDCR = blkproc(DCR,[8 8],'round(round(x)./P1)',QC);

%end of compression
%========================================================
%start decompression

% Dequantize DCT coefficients
  dDY = blkproc(qDY,[8 8],'x.*P1',QY);
  dDCB = blkproc(qDCB,[8 8],'x.*P1',QC);
  dDCR = blkproc(qDCR,[8 8],'x.*P1',QC);

% Inverse DCT
  iY = blkproc(dDY./qmax, [8 8],'P1*x*P2',M',M);
  iCB = blkproc(dDCB./qmax, [8 8],'P1*x*P2',M',M);
  iCR = blkproc(dDCR./qmax, [8 8],'P1*x*P2',M',M);
     
% Up-sample chroma
  uf1=[1 3 3 1] / 4; %1D filter
  uf2=uf1'*uf1; %2D filter
   % array padding (replicate borders)
   aCB=zeros(L+2,L+2); aCB(2:L+1,2:L+1)=iCB;
   aCB(2:L+1,1)=iCB(:,1); aCB(2:L+1,L+2)=iCB(:,L);
   aCB(1,:)=aCB(2,:); aCB(L+2,:)=aCB(L+1,:);
   aCR=zeros(L+2,L+2); aCR(2:L+1,2:L+1)=iCR;
   aCR(2:L+1,1)=iCR(:,1); aCR(2:L+1,L+2)=iCR(:,L);
   aCR(1,:)=aCR(2,:); aCR(L+2,:)=aCR(L+1,:);
   % upsampling
   M=2*(L+2);
   uCB=zeros(M,M); 
   uCB(1:2:M-1,1:2:M-1)=aCB;
   rCB=conv2(uf2,uCB);
   uCR=zeros(M,M); 
   uCR(1:2:M-1,1:2:M-1)=aCR;
   rCR=conv2(uf2,uCR); 
   
   rCB = rCB(4:size(rCB,1)-4, 4:size(rCB,2)-4);
   rCR = rCR(4:size(rCR,1)-4, 4:size(rCR,2)-4);
  
% Concatenate and convert to RGB
JF = ycbcr2rgb(cat(3, iY, rCB, rCR));

% ensure range (0..1)
  m1=min(min(JF(:,:,1))); M1=max(max(JF(:,:,1)));
  JF(:,:,1)=(JF(:,:,1)-m1)/(M1-m1);
  m2=min(min(JF(:,:,2))); M2=max(max(JF(:,:,2)));
  JF(:,:,2)=(JF(:,:,2)-m2)/(M2-m2);
  m3=min(min(JF(:,:,3))); M3=max(max(JF(:,:,3)));
  JF(:,:,3)=(JF(:,:,3)-m3)/(M3-m3);
  
% "Energies"  
  EO=round(sum(sum(abs(F)))); %original
  ECY=sum(sum(abs(qDY))); %compressed Y
  ECB=sum(sum(abs(qDCB))); %compressed CB
  ECR=sum(sum(abs(qDCR))); %compressed CR
  EJ=round(sum(sum(abs(JF)))); %recovered
  
  %display
  figure(1)
  imagesc(F);
  title('original photo');
  
  figure(2)
  imagesc(JF);
  title('recovered photo');
  
  figure(3)
  subplot(1,3,1)
  bar(squeeze(EO)); title('R-G-B original photo')
  ylabel('Energy');
  
  subplot(1,3,2)
  bar(squeeze(EJ)); title('R-G-B recovered photo')
  ylabel('Energy');
  
  subplot(1,3,3)
  bar([ECY ECB ECR]); title('Y-CB-CR compressed photo')
  ylabel('Energy');
  
  
  
  
 