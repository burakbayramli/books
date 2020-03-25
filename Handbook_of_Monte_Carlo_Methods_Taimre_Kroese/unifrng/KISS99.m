% KISS99.m
% Seeds: Correct variable types crucial!
A=uint32(12345); B=uint32(65435); Y=12345; Z=uint32(34221);
N=100; % Compute the sequence for N steps
U=zeros(1,N);
for t=1:N
    % Two Multiply with Carry Generators
    A=36969*bitand(A,uint32(65535))+bitshift(A,-16);
    B=18000*bitand(B,uint32(65535))+bitshift(B,-16);
    % MWC: Low and High 16 bits are A and B
    X=bitshift(A,16)+B;
    % CONG: Linear Congruential Generator
    Y = mod(69069*Y+1234567,4294967296);
    % SHR3: 3-Shift Register Generator
    Z=bitxor(Z,bitshift(Z,17));
    Z=bitxor(Z,bitshift(Z,-13));
    Z=bitxor(Z,bitshift(Z,5));
    % Combine them to form the KISS99 generator
    KISS=mod(double(bitxor(X,uint32(Y)))+double(Z),4294967296);
    U(t)=KISS/4294967296; % U[0,1] output
end

