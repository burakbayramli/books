"""Convert structured text to LaTeX.

*LaTeX* - A class that converts structured text (cf. the "doc-sig")
          into a format readable by LaTeX. Based on the class
          *HTML* authored by Jim Fulton which appears in
          *StructuredText.py*.



Usage (this is long and rambling so I can test it with itself...):

    1. Put *struct2latex.py* someplace that python and can find it.

    2. Create your LaTeX file by:
    
        a. Creating a **LaTeX** object (e.g., 'st = LaTeX(string)').

        b. Getting the LaTeXified string by converting the **LaTeX
        object to a string (.e.g, 'lt = str(st)' or 'print st').

        c. Save your LaTeXified string somewhere.

    3. You should be able to include the LaTeX text in any
    LaTeX file.  Two ways I use it are:

        * Use the text by itself by putting it in a stub
        file. For example::

            \\documentstyle[11pt]{article}
            
            \\begin{document}
            
            \\include{docstring}
            
            \\end{document}

        * I'm using use it to support structured text in *py2tex*.

    4. Run LaTeX.

    5. Once you have a dvi file your on your own....

There are some caveats (of course):

   Characters -- I believe all the LaTeX special characters
   (&%#_{}~^\) should be properly escaped (with the exception of
   $ - see below, but no guarantees.
       * And now it should allow bullet lists that are adjacent to work.
       * This is provided by the magic of regsub.gsub.
       * But who knows it may have some horrible side effects...

   Equations -- I thought, ``as long as we're using LaTeX, we
   should have access to equations.'' So, '$' is used to invoke
   math mode, just as in LaTeX. For example, '$x = \oint y\,dy$'
   produces $x = \oint y\,dy$. $ obeys the same rules as ', so you
   usually shouldn't have to quote it - although that would
   probably be safer...

   Quotes -- The normal LaTeX style quotes work fine as long as
   there is no white space inside the
   quote ( ' ). 

.. "doc-sig" http://www.python.org/sigs/doc-sig/
"""

import regex, regsub, string
import StructuredText
ST = StructuredText

href_re      = regex.compile('[.][.] \(".+"\)[ \t]*\(.*\)\n')
line2_re     = regex.compile('.*\n\([ \t]*\n\)*\([ \t]*\)') 
slashable_re = regex.compile('[$&%#_{}]')
quotable_re  = regex.compile('[~^\\]')
eqn_re = regex.compile("[ \t\n(]$\([^ \t$]\([^\n']*[^ \t']\)?\)$\([) \t\n,.:;!?]\)")
carrot_re = regex.compile("\\^")

expand_bullet=regex.compile('\n[ \t\n]*[o*-][ \t\n]')
expand_deflist=regex.compile('\n[ \t\n]*[^\n]+[ \t]+--[ \t\n]')

def _split(s):
    """Split a string into normal and quoted pieces.

    Splits a string into normal and quoted (or math mode)
    sections. Returns a list where the even elements are normal
    text, and the odd elements are quoted. The appropiate quote
    tags ($ and \\verb) are applied to the quoted text.

    """
    r = []
    while 1:
	epos = eqn_re.search(s)
	qpos = ST.code.search(s)
	if epos == qpos:		## == -1
	    break
	elif (qpos == -1) or (epos != -1 and epos < qpos):
	    r.append(s[:epos])
	    end = epos + eqn_re.match(s[epos:])
	    arg = [eqn_re.group(1), eqn_re.group(3)]
	    if not arg[1]: arg[1] = ''
	    r.append( ' $%s$%s ' % tuple(arg))
	else:				## (epos==-1) or (qpos != -1 and epos > qpos):
	    r.append(s[:qpos])
	    end = qpos + ST.code.match(s[qpos:])
	    arg = [regsub.gsub(carrot_re, '^\\verb@\\0@\\verb^', ST.code.group(1)),
		   ST.code.group(3)]
	    if not arg[1]: arg[1] = ''
	    r.append(' \\verb^%s^%s ' % tuple(arg))
	s = s[end:]
    r.append(s)
    return r


def _ctag(str, hrefs=()):
    """Quote, tag, and escape the text.

    This is a modified version of the 'ctag' function appearing in
    StructuredText.py. The differences include, 
      * it uses _split, so that it avoids escaping text in quotes or
      in math-mode.
      * it processes hrefs.
      * it escapes LaTeX special characters.
      * it doesn't try to find duplicate list items - that got moved
      into LaTeX.

    """
    if str is None: str = ''
    str = ' %s' % str			# prepend a space 
    str = _split(str)
    for i in xrange(len(str)): 
	if not i%2:
	    str[i]=regsub.gsub(quotable_re, '\\verb@\\0@', str[i])
	    str[i]=regsub.gsub(slashable_re, '\\\\\\0', str[i])
	    str[i]=regsub.gsub(ST.strong,' {\\bfseries \\1}\\2', str[i])
	    str[i]=regsub.gsub(ST.em,' {\\itshape \\1}\\2',str[i])
	    for ref, link in hrefs:
		tag = '{\slshape %s}\\footnote{%s}' % (ref[1:-1], link)
		str[i] = string.joinfields(string.split(str[i], ref), tag)
    return string.joinfields(str)


def _strip_hrefs(string):
    """Strip hrefs out of a string.

    Strip the hrefs of the form '.. "tag" url' out of
    *string*. Return string, as well as a dictionary containing the
    stripped references.

    """
    hrefs = []
    s = string
    l = href_re.search(s)
    while l != -1:
	hrefs.append(href_re.group(1,2))
	s = s[l+1:]
	l = href_re.search(s)
    string = regsub.gsub(href_re, '', string)
    return string, hrefs


def _separate_bullets(string):
    """Separate list items by a newline."""
    string = regsub.gsub(expand_bullet, '\n\\0', string)
    string = regsub.gsub(expand_deflist, '\n\\0', string)
    return string


class LaTeX(ST.StructuredText):

    """Translate StructuredText to LaTeX.

    This is loosely based on Jim Fulton's class 
    HTML.
    """

    def __init__(self, aStructuredString, level=1, isdoc=1):
	"""Create a LaTeX object."""

	self.level = level
	aStructuredString = ST.untabify(aStructuredString)
	if isdoc:
	    if line2_re.match(aStructuredString) != -1:
		aStructuredString = line2_re.group(2) + aStructuredString
	    aStructuredString, self.hrefs = _strip_hrefs(aStructuredString)
	    aStructuredString = _separate_bullets(aStructuredString)
	paragraphs = regsub.split(aStructuredString, ST.paragraph_divider)
	paragraphs = map(ST.indent_level, paragraphs)
	self.structure = ST.structure(paragraphs)


    def _str(self,structure,level):
	"""Translate *structure* to LaTeX.

	Driver for the translation. Based on HTML._str.
	Differences include:

	1. changed the handling of examples so that bullets could
	have examples too.

	"""
	if type(structure) == type(''):
	    return structure
	r=''
	for s in structure:
	    ##print s[0],'\n', len(s[1]), '\n\n'
	    if ST.example.search(s[0]) >= 0 and s[1]:
		s0, s1 = s[0], self.pre(s[1])
	    elif s[0][-2:]=='::' and s[1]:
		s0, s1 = s[0][:-1], self.pre(s[1])
	    else:
		s0, s1 = s[0], s[1]
	    #
	    if ST.bullet.match(s0) >= 0:
		p=ST.bullet.group(1)
		r=self.ul(r,p,self._str(s1,level))
	    elif ST.ol.match(s0) >= 0:
		p=ST.ol.group(3)
		r=self.ol(r,p,self._str(s1,level))
	    elif ST.olp.match(s0) >= 0:
		p=ST.olp.group(1)
		r=self.ol(r,p,self._str(s1,level))
	    elif ST.dl.match(s0) >= 0:
		t,d=ST.dl.group(1,2)
		r=self.dl(r,t,d,self._str(s1,level))
	    elif ST.nl.search(s0) < 0 and s1:
		# Treat as a heading
		t=s0
		r=self.head(r,t,level,self._str(s1,level+1))
	    else:
		r=self.normal(r,s0,self._str(s1,level))
	return r

    def ul(self, before, p, after):
	"""Process an unordered list."""
	if before[-14:] == '\\end{itemize}\n':
	    return ('%s\n\\item %s%s\n\n\\end{itemize}\n' % 
		    (before[:-15],_ctag(p, self.hrefs),after))
	else:
	    return ('%s\\begin{itemize}\n\n\\item %s%s\n\n\\end{itemize}\n'
		    % (before,_ctag(p, self.hrefs),after))

    def ol(self, before, p, after):
	"""Process an ordered list."""
	if before[-16:] == '\\end{enumerate}\n':
	    return ('%s\n\\item %s%s\n\n\\end{enumerate}\n' % 
		    (before[:-16],_ctag(p, self.hrefs),after))
	else:
	    return ('%s\\begin{enumerate}\n\n\\item %s%s\n\n\\end{enumerate}\n'
		    % (before,_ctag(p, self.hrefs),after))

    def dl(self, before, t, d, after):
	"""Process a description list."""
	if before[-18:] == '\\end{description}\n':
	    return ('%s\n\\item[%s]%s%s\n\n\\end{description}\n' % 
		    (before[:-18], _ctag(t, self.hrefs), _ctag(d, self.hrefs),after))
	else:
	    return ('%s\\begin{description}\n\n\\item[%s]%s%s\n\n\\end{description}\n'
		    % (before,_ctag(t, self.hrefs),_ctag(d, self.hrefs),after))

    def head(self, before, t, level, d):
	"""Process a heading."""
	t="{\\bfseries %s }" % _ctag(t, self.hrefs)
	return ('%s\\begin{description}\n\\item[%s]\\ \n\n%s\n\\end{description}\n'
	        % (before,t,d))

    def normal(self,before,p,after):
	"""Process a normal paragraph."""
	return '%s\n%s\n%s\n' % (before,_ctag(p, self.hrefs),after)

    def pre(self,structure,tagged=0):
	"""Process some pre-formatted (example) text."""
	if not structure: return ''
	if tagged:
	    r=''
	else:
	    r='\\begin{verbatim}\n'
	for s in structure:
	    r="%s%s\n\n%s" % (r,s[0],self.pre(s[1],1))
	if not tagged: r=r+'\\end{verbatim}\n'
	return r
	
    def __str__(self):
	"""Return the translated text."""
	return self._str(self.structure,self.level)


if __name__ == '__main__':
    print LaTeX(__doc__)


















