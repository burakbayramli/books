function [] = simple_system_data()
    T = 4;
    p = 0.3;
    
    U_seq = GenerateSequences([1 2],T);
    W_seq = GenerateSequences([1 2],T);
    pw = (p.^sum(W_seq == 1) .* (1-p).^(sum(W_seq == 2)))';
    phi_cl = cell(2^(2*2*(T-1)),1);
    for k = 1:2^(2*2*(T-1))
        z = 2^4*(k-1)+(2^4-1);
        phi_cl{k} = reshape(dec2bin(z,2*2*T)-'0'+1,2,2,T);
    end
    
    assignin('caller' , 'T' , T);
    assignin('caller' , 'p' , p);
    assignin('caller' , 'a' , 10);
    assignin('caller' , 'x0' , 1);
    assignin('caller' , 'xf' , 2);
    assignin('caller' , 'ct' , ((1:T)').^2);
    assignin('caller' , 'U_seq' , U_seq);
    assignin('caller' , 'W_seq' , W_seq);
    assignin('caller' , 'pw' , pw);
    assignin('caller' , 'phi_cl' , phi_cl);
end

function S = GenerateSequences(X,L)
    switch L
        case 0
            S = [];
        case 1
            S = X;
        otherwise
            R = GenerateSequences(X,L-1);
            S = [];
            for x = X
                S = [S , [R ; repmat(x,1,size(R,2))]];
            end
    end
end
