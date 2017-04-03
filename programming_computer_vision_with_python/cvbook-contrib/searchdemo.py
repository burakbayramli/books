import cherrypy, os, random
import cPickle as pickle

import imagesearch
import imtools

"""After ch07_buildindex.py has built an index in test.db, this queries it over
a web service.
"""

class SearchDemo(object):
  def __init__(self):
    self.imlist = imtools.get_imlist(
        '/Users/thakis/Downloads/ukbench/first1000')[:100]
    self.ndx = range(len(self.imlist))

    with open('vocabulary.pkl', 'rb') as f:
      self.voc = pickle.load(f)

    self.maxresults = 15

    self.header = """\
<!doctype html>
<html>
<head><title>Image search example</title></head>
<body>"""

    self.footer = """\
</body>
</html>"""

  @cherrypy.expose
  def index(self, query=None):
    self.searcher = imagesearch.Searcher('test.db', self.voc)

    html = self.header
    html += """\
  <br>
  Click an image to search. <a href="?query=">Random selection</a> of images.
  <br><br>"""
    if query:
      res = self.searcher.query(query)[:self.maxresults]
      for dist, ndx in res:
        imname = self.searcher.get_filename(ndx)
        html += '<a href="?query=%s">' % imname
        html += '<img src="/img/%s" width=100>' % os.path.basename(imname)
        html += '</a>'
    else:
      random.shuffle(self.ndx)
      for i in self.ndx[:self.maxresults]:
        imname = self.imlist[i]
        html += '<a href="?query=%s">' % imname
        html += '<img src="/img/%s" width=100>' % os.path.basename(imname)
        html += '</a>'
    html += self.footer
    return html

config = { '/img': {
    'tools.staticdir.on': True,
    'tools.staticdir.dir': '/Users/thakis/Downloads/ukbench/first1000',
}}

cherrypy.quickstart(SearchDemo(), '/', config=config)
