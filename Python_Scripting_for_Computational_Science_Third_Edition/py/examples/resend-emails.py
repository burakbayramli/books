#!/usr/bin/env python
import mailbox  # read various mailbox formats (incl. Unix format)
import smtplib  # tools for sending mail
import string

# open a part of my outgoing mailbox, for resending:
mbox = mailbox.UnixMailbox(open("testbox","r"))
while 1:
    msg = mbox.next()   # grab next message
    if not msg: break
    # can extract mail header fields like a dictionary,
    # e.g. msg['To'], msg['From'], msg['cc'], msg['date'] etc.
    # msg.keys() lists all keys for this message
    # msg.fp.read() gets the body of the message
    # str(msg) is the header of the message

    # send message to msg['To'], those on the cc list, and myself:
    to = [msg['To']]
    if msg.has_key('cc'):
        to += map(string.strip,msg['cc'].split(','))
    to.append(msg['From'])  # add myself
    
    message = str(msg) + """

Due to an error with my email connection, the email I sent
you on %s may not have reached you.
A copy of the message is inserted below.
I apologize if you end up with multiple copies of this message.


===============================================================================

""" % (msg['date']) + msg.fp.read()
    mailserver = 'ulrik.uio.no'
    server = smtplib.SMTP(mailserver)
    server.sendmail(msg['From'], to, message)
    # test: send to myself only...
    # server.sendmail(msg['From'], [msg['From']], message)
    server.quit()
    print "have resent mail to", to

