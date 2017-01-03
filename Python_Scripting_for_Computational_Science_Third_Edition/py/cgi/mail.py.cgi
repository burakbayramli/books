#!/usr/local/bin/python
import cgi, os
print 'Content-type: text/html\n'
form = cgi.FieldStorage()
address = ''
note = ''
if form.has_key('mailaddress'):  
    mailaddress = form.getvalue('mailaddress')
    note = 'Thank you!'
    # send a mail using os.popen to write input data
    # to a program (/usr/bin/sendmail):
    mail = os.popen('/usr/lib/sendmail ' + mailaddress, 'w')
    mail.write("""
To: %s
From: me
%s
""" % (mailaddress, note))
    mail.close()  # execute sendmail command
    
# print form where the user can fill in a mail address:
print """
<HTML><BODY BGCOLOR="white">
<FORM ACTION="mail.cgi" METHOD="POST">
Please give your email address: 
<INPUT TYPE="text" NAME="mailaddress" SIZE="10" VALUE="%s">
<INPUT TYPE="submit" VALUE="equals" NAME="equalsbutton"> %s
</FORM></BODY></HTML>
""" % (mailaddress, note)
