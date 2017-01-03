#!/usr/bin/env python
import sys, os, os.path, string, re, glob

try:
    q4wfilename = sys.argv[1]
except:
    print 'Usage: %s inputfile.q4w [languagefile]' % sys.argv[0]
    print '(languagefile can be language.en (default), language.no, ...)'
    print 'Just make a .q4w file with questions and answers in a directory;'
    print 'q4w-generate.py will copy the necessary files for you.'
    sys.exit(1)
if not re.search(r'\.q4w$', q4wfilename):  # .q4w suffix?
    print 'Input file', q4wfilename, 'must end with .q4w'
    sys.exit(1)
else:
    filenamebase = re.sub(r'\.q4w', '', q4wfilename)

# load q4w input file into the lines list:
file = open(q4wfilename, 'r'); lines = file.readlines(); file.close()

# create a subdirectory filenamebase and place all necessary files there:
import shutil
if os.path.isdir(filenamebase):
    shutil.rmtree(filenamebase)
os.mkdir(filenamebase, 0755)
os.chdir(filenamebase)

# copy necessary files:
import shutil
def copy(file):
    if os.environ.has_key('scripting'):
        fullname = os.path.join(os.environ['scripting'],
                                'src','py','examples', 'q4w', file)
        if not os.path.isfile(fullname):
            print fullname, 'does not exist - ??'; sys.exit(1)
        else:
            shutil.copy(fullname, file)
    else:
        # we do not have the scripting software available,
        # assume that the q4w files are collected in a directory
        # (the files will then be copied from this (parent) dir)
        fullname = os.path.join(os.pardir,file)
        if not os.path.isfile(fullname):
            print fullname, 'does not exist - ??'; sys.exit(1)
        else:
            shutil.copy(fullname, file)

copy('q4w-record.cgi')
copy('q4w-statistics.cgi')
copy('colorline.gif')
copy('percent.gif')
copy('QAdata.py')
shutil.copy(os.path.join(os.pardir,q4wfilename), os.curdir)

# we need language-specific text for the submit and reset buttons:
try:
    languagefile = sys.argv[2]
    copy(languagefile)
except:
    languagefile = 'language.en'  # English language is default
    copy('language.en')
    
if not os.path.isfile(languagefile):
    print 'The file %s is not found' % languagefile
try:
    execfile(languagefile)  # initialize the translate dictionary
except:
    print 'The file %s contains an error!' % languagefile
    sys.exit(1)

# HTML file with the resulting form:
htmlfile = open(filenamebase+'.html', 'w')

# print heading in HTML file:
htmlfile.write("""
<!-- file automatically generate by q4w-generate.py -->
<!-- from input file %s.q4w -->
<HTML>
<HEAD>
<TITLE>%s: %s</TITLE>
</HEAD>
<BODY BGCOLOR="white">
<FORM ACTION="q4w-record.cgi" METHOD="POST">
""" % (filenamebase, translate['Questionnaire'], filenamebase))

try:
    sys.path.insert(0, os.path.join(os.environ['scripting'],'src','py','examples','q4w'))
except: pass
from QAdata import *

def writeform(answers, formtype, question_no, file,
              text_width=40, text_height=5):
    if formtype == 'single-choice':
        for answer in answers:
            file.write("""<INPUT TYPE="radio" NAME="%s" """\
                       """VALUE="%s">&nbsp; %s <BR>\n""" % \
                       (question_no, answer, answer))
    elif formtype == 'multiple-choice':
        for answer in answers:
            file.write("""<INPUT TYPE="checkbox" NAME="%s" """\
                       """VALUE="%s">&nbsp; %s <BR>\n""" % \
                       (question_no, answer, answer))
    elif formtype == 'free-text':
        file.write("""<TEXTAREA COLS=%d ROWS=%d WRAP="hard" """\
                   """NAME="%s"></TEXTAREA>\n""" % \
                   (text_width, text_height, question_no))
    elif formtype == 'one-line-text':
        file.write("""<INPUT TYPE="text" NAME="%s" """\
                   """SIZE="%d">\n""" % (question_no, text_width))
    elif formtype == 'pull-down-menu':
        file.write("""<SELECT NAME="%s" SIZE=1>\n""" % question_no)
        for answer in answers:
            file.write("""<OPTION VALUE="%s"> %s </OPTION>\n""" \
                        % (answer, answer))
        file.write("""</SELECT><BR>\n""")
    else:
        print 'answer type',answer_type,'not implemented'
        sys.exit(1)

qa = []  # array of QAdata objects

last_tag = ''  # used to check the syntax

question_counter = 0
for line in lines:
    if re.search(r'^\s*\#.*', line):      # comment line?
        line = string.strip(line)
        # write line except opening '#' ('\n' is already
        # stripped by string.strip),  inside HTML comment signs:
        htmlfile.write('<!-- ' + line[1:] + ' -->\n')
        last_tag = '#'
        #print 'DEBUG: found a comment:',line[1:-1]
#   elif re.search(r'^(Q|A|T|):', line):  # Q: or A: or T:  ?
    elif re.search(r'^((Q|A|T|):|\|)', line):  # Q: or A: or T: or | ?
        title_line = re.search(r'^T:(.*)', line)
        if title_line:
            title = string.strip(title_line.group(1))
            #print 'DEBUG: found a title:',title
            # write title centered with nice colorlines around it:
            htmlfile.write('<CENTER>\n<IMG SRC="colorline.gif">\n')
            htmlfile.write('<H1>%s</H1>\n' % title)
            htmlfile.write('<IMG SRC="colorline.gif">\n</CENTER>\n')
            last_tag = 'T'

        question_line = re.search(r'^Q:(.*)', line)
        if question_line:
            question_counter = question_counter + 1
            question = string.strip(question_line.group(1))
            qa.append(QAdata(question, question_counter))
            #print 'DEBUG: found a question:',question
            last_tag = 'Q'

            # write HTML code:
            if question_counter == 1:   # start list of questions:
                htmlfile.write('\n\n<OL>\n')
            htmlfile.write('<P><LI><H4>'+question+'</H4>\n\n')

        # questions (Q:) can be continued by lines starting with |
        cont_q = re.search(r'^\|(.*)', line)
        if cont_q:
            # copy to output:
            text = cont_q.group(1)
            qa[question_counter-1].question = qa[question_counter-1].question \
                                              + '\n' + text
            htmlfile.write(text + '\n')

        answer_line = re.search(r'^A:\s*([A-Za-z\-]+)[\s\n]+(.*)', line)
        # note: the regex above allows 2nd group to be empty
        # (important for free-text and one-line-text answers)
        if answer_line:
            answer_type = answer_line.group(1)
            if answer_type == 'textarea' or \
               answer_type == 'one-line-text':
                answers = []
            else:
                answers = re.split(r'\s*\|\s*',
                                   string.strip(answer_line.group(2)))
            #print 'DEBUG: found an answer of type',answer_type,'list=',answers
            if not last_tag == 'Q':
                print 'Error in %s when reading line\n\t%s  ' \
                      'last tag was %s:, should be Q:' %\
                      (q4wfilename,line,last_tag)
            qa[question_counter-1].specifyAnswers(answers, answer_type)

            # write HTML code:
            writeform(answers, answer_type, question_counter, htmlfile)
    else:
        # some ordinary text that we copy "as is" to the HTML file,
        # but first check if it could be a tag with typo:
        m = re.search(r'^([A-Za-z]:)', line)
        if m:
            print 'Found a line starting with',m.group(1),\
                  ' (unknown tag), might be a typo'
        htmlfile.write(line)


htmlfile.write("""
</OL>  <!-- end of list of questions -->

<!-- transfer the basename of filenames through a "hidden" form -->
<INPUT TYPE="hidden" NAME="filenamebase" VALUE="%s">
<P>
<!-- the standard submit and reset buttons at the end -->
<INPUT TYPE="submit" VALUE="%s" NAME="submitbutton">
<INPUT TYPE="reset"  VALUE="%s"  NAME="resetbutton">
</FORM>

</BODY>
</HTML>
""" % (filenamebase, translate['Submit'], translate['Reset']))
htmlfile.close()

# save the data structure on the .dbsum file:
dbfile = open(filenamebase+'.dbsum', 'w')
for item in qa:
    # first write a record separator:
    dbfile.write('#------- question %d -------\n' % item.question_no)
    dbfile.write(item.write())
dbfile.close()

# test pickle:
#dbfile = open(filenamebase+'.pickle', 'w')
#import pickle
#pickle.dump(qa, dbfile)
#dbfile.close()

# make sure that the CGI scripts are executable:
os.chmod('q4w-record.cgi', 0755)
os.chmod('q4w-statistics.cgi', 0755)
# make sure the datafiles are writeable for all users:
os.chmod(filenamebase+'.dbsum', 0777)
dbfile = open(filenamebase+'.dball', 'w')
dbfile.close()
os.chmod(filenamebase+'.dball', 0777)
dbfile = open('ans.html', 'w')
dbfile.close()
os.chmod('ans.html', 0777)

# write a little script chmod.files that opens files in case
# the directory is moved to another machine and protections
# are lost:
f = open('chmod.files', 'w')
f.write("""
#!/bin/sh
# make scripts and database files in the current directory
# executable and accessible for CGI scripts:
chmod a+rx q4w-record.cgi q4w-statistics.cgi
chmod a+rxw %s %s ans.html
""" % (filenamebase+'.dbsum', filenamebase+'.dball'))
f.close()
os.chmod('chmod.files', 0755)

# write a little script reset that resets the database
# by making empty .dball and ans.html file and copying
# .dbsum.0 to .dbsum
shutil.copy(filenamebase+'.dbsum',filenamebase+'.dbsum'+'.0')
f = open('reset_database', 'w')
f.write("""\
#!/bin/sh -x
# reset database
cp %(filenamebase)s.dbsum.0 %(filenamebase)s.dbsum
rm %(filenamebase)s.dball ans.html
touch %(filenamebase)s.dball ans.html
chmod a+rwx %(filenamebase)s.dbsum %(filenamebase)s.dball ans.html *.cgi
"""
        % vars())
f.close()
os.chmod('reset_database', 0755)


print """
%s.q4w was successfully translated to a web form: %s.html
in the new subdirectory %s
""" % (filenamebase,filenamebase,filenamebase)

