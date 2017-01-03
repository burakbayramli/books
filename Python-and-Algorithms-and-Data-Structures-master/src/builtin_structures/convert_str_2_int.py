#!/usr/bin/env python

__author__ = "bt3"


def conv_int2str(int1):
    '''
    >>> conv_str2int('367')
    367
    >>> conv_str2int('0')
    0
    >>> conv_str2int('-10')
    -10
    >>> conv_str2int('1e5')
    100000
    '''

    aux_dict = {key:value for key in range(10) for value in '0123456789'[key]}

    if int1 == 0:
        return '0'
    elif int1 < 0:
        sign = '-'
        int1 = int1*(-1)
    else:
        sign = ''


    aux_ls = []

    while int1 > 0:
        c = int1%10
        int1 = int1//10
        cadd = aux_dict[c]
        aux_ls.append(cadd)

    aux_ls.reverse()

    return sign + ''.join(aux_ls)



def conv_str2int(str1):
    '''
    >>> conv_int2str(0)
    '0'
    >>> conv_int2str(1e5)
    '100000'
    >>> conv_int2str(367)
    '367'
    >>> conv_int2str(-10)
    '-10'
    '''
    if not str1:
        return None

    aux_dict = {key:value for value in range(10) for key in '0123456789'[value]}

    if str1[0] == '-':
        sign = -1
        str1 = str1[1:]
    else:
        sign = 1

    dec, result = 1, 0

    for i in range(len(str1)-1, -1, -1):
        aux = str1[i]
        if aux == 'e':
            exp_num = conv_str2int(str1[i+1:])
            number = conv_str2int(str1[:i])
            result = number*10**exp_num
            break
        result += aux_dict[aux]*dec
        dec *= 10

    return result*sign


if __name__ == '__main__':
    import doctest
    doctest.testmod()

