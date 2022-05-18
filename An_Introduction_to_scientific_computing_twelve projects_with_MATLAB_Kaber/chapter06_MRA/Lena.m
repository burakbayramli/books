%     lecture de l'image
      close all
      [U,MAP]=imread('lenna.jpg','jpg');
      colormap('gray');
      axis equal;
      imagesc(U);
      title('Image initiale');
      CJ=double(U);
      %CJ=(CJ+1)/4;
      figure
      colormap('gray');
      axis equal;
      imagesc(CJ);
      title('Coefficients');
      N=size(CJ,1);
      J=8;J=13;
      for lin=1:N
         CJ(lin,:)=CodageDaubechie(J,CJ(lin,:));
      end
      for col=1:N
         CJ(:,col)=CodageDaubechie(J,CJ(:,col));
      end
      figure
      colormap('gray');
      axis equal;
      imagesc(CJ);
      title('Details');
      
      epsilon=N^2*0.001;
      [CJ,somme]=Seuillage(J,CJ,epsilon);
      
      for col=1:N
         CJ(:,col)=DecodageDaubechie(J,CJ(:,col));
      end
      for lin=1:N
         CJ(lin,:)=DecodageDaubechie(J,CJ(lin,:));
      end
      figure
      colormap('gray');
      axis equal;
      imagesc(CJ);
      title('Reconstruction');
      somme
      