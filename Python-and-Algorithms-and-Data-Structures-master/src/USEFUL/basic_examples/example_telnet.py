#!/usr/bin/env python


__author__ = "bt3"


from telnetlib import Telnet


# examples of telnet connections
PORT = 12345
HOST = '54.209.5.48'

# creating connection
tn = Telnet(HOST ,PORT)

# reading input
msg_in2 = tn.read_all().dec_msg()
tn.read_until(b'psifer text: ')

# writing outputs
tn.write(msg.encode() + b'\n')
