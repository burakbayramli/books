#!/usr/bin/env python


__author__ = "bt3"


import socket


def netcat(hostname, port, content):

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    s.connect((hostname, port))
    s.sendall(content)

    adata = []
    while 1:
        data = s.recv(1024)
        if data == '':
            break
        adata.append(data)

    s.close()
    return adata



if __name__ == '__main__':

    PORT = 12345
    HOSTNAME = '54.209.5.48'

    message = netcat(HOSTNAME, PORT, 'Hello!')[1]
    print message