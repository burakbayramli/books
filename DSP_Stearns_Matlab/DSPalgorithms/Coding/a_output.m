function [y,Nbits,Nbtf]=a_output(y,Nbits,bit,Nbtf,ipr)
% Function used by a_encode to stuff bits into y (uint*8).
%
% values of y,Nbits,and Nbtf] are altered.
% y =uint8 vector.
% Nbits =next bit to be used in y. starts at 0.
%        storage goes from left to right in each byte of y.
% Nbtf  =# bits to follow is always returned =0.
% bit =bit to be stored.
% ipr has the same effect here as in a_encode when =2.
sh=[128 64 32 16 8 4 2 1];
iy=fix(Nbits/8)+1;                          %iy =index - designates y(iy)
if iy>length(y),
	error('Vector y has been exceeded.')
end
ibit=Nbits-8*(iy-1);                        %ibit =bit within y(iy); 0-7
y(iy)=y(iy)+bit*sh(ibit+1);
Nbits=Nbits+1;
if ipr==2,
	fprintf([blanks(60),'Nbits, Bit out: %4.0f%3.0f\n'],Nbits,bit);
end
while Nbtf>0;
   iy=fix(Nbits/8)+1;
   ibit=Nbits-8*(iy-1);
   y(iy)=y(iy)+(1-bit)*sh(ibit+1);
   Nbits=Nbits+1;
	if ipr==2,
   	fprintf([blanks(60),'Nbits, Bit adj: %4.0f%3.0f\n'],Nbits,1-bit);
	end
   Nbtf=Nbtf-1;
end
