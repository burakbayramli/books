function [ twist ] = se3Log( T )
    lg = logm(T);
    twist = [lg(1,4) lg(2,4) lg(3,4) lg(3,2) lg(1,3) lg(2,1)]';
end

