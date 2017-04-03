import cPickle as pickle
import sqlite3

class Indexer(object):
  def __init__(self, db, voc):
    """Initialize with the name of the database and a vocabulary object."""
    self.con = sqlite3.connect(db)
    self.voc = voc

  def __del__(self):
    self.con.close()

  def db_commit(self):
    self.con.commit()

  def create_tables(self):
    self.con.execute('create table imlist(filename)')
    self.con.execute('create table imwords(imid, wordid, vocname)')
    self.con.execute('create table imhistograms(imid, histogram, vocname)')

    self.con.execute('create index im_idx on imlist(filename)')
    self.con.execute('create index wordid_idx on imwords(wordid)')
    self.con.execute('create index imid_idx on imwords(imid)')
    self.con.execute('create index imidhist_idx on imhistograms(imid)')
    self.db_commit()

  def add_to_index(self, imname, descr):
    """Take an image with feature descriptors, project on vocabulary, and add to
    database."""
    if self.is_indexed(imname):
      return
    print 'indexing', imname  # FIXME: remove

    imid = self.get_id(imname)

    imwords = self.voc.project(descr)
    word_count = imwords.shape[0]

    # Link each word to image.
    for i in range(word_count):
      word = imwords[i]
      self.con.execute('insert into imwords(imid, wordid, vocname) '
          'values (?, ?, ?)', (imid, word, self.voc.name))

    # Store word histogram for image
    self.con.execute('insert into imhistograms(imid, histogram, vocname) '
        'values (?, ?, ?)', (imid, pickle.dumps(imwords), self.voc.name))

  def is_indexed(self, imname):
    im = self.con.execute(
        'select rowid from imlist where filename = ?', (imname,)).fetchone()
    return im is not None

  def get_id(self, imname):
    res = self.con.execute('select rowid from imlist where filename = ?',
                           (imname,)).fetchone()
    if res is not None:
      return res[0]
    cur = self.con.execute('insert into imlist(filename) values (?)',
                           (imname,))
    return cur.lastrowid


class Searcher(object):
  def __init__(self, db, voc):
    """Initialize with the name of the database and a vocabulary object."""
    self.con = sqlite3.connect(db)
    self.voc = voc

  def __del__(self):
    self.con.close()

  def candidates_from_word(self, imword):
    """Get list of images containing imword."""
    im_ids = self.con.execute(
        'select distinct imid from imwords where wordid = ?',
        (int(imword),)).fetchall()
    return [i[0] for i in im_ids]

  def candidates_from_histogram(self, imwords):
    """Get list of images with similar words."""
    words = imwords.nonzero()[0]
    candidates = []
    for word in words:
      candidates += self.candidates_from_word(word)

    # Take all unique words and reverse-sort on occurrence.
    tmp = [(w, candidates.count(w)) for w in set(candidates)]
    tmp.sort(key=lambda x: x[1], reverse=True)

    # Return sorted list, best matches first.
    return [w[0] for w in tmp]

  def get_imhistogram(self, imname):
    """Return the word histogram for an image."""
    imid = self.con.execute('select rowid from imlist where filename = ?',
                             (imname,)).fetchone()
    s = self.con.execute('select histogram from imhistograms where rowid = ?',
                         imid).fetchone()
    return pickle.loads(str(s[0]))

  def get_filename(self, imid):
    return self.con.execute(
        'select filename from imlist where rowid = ?', (imid,)).fetchone()[0]

  def query(self, imname):
    """Find a list of matching images for imname."""
    import numpy
    h = self.get_imhistogram(imname)
    candidates = self.candidates_from_histogram(h)

    matchscores = []
    for imid in candidates:
      cand_h = self.get_imhistogram(self.get_filename(imid))
      cand_dist = numpy.sqrt(numpy.sum((h - cand_h)**2))
      matchscores.append((cand_dist, imid))
    matchscores.sort()
    return matchscores


def compute_ukbench_score(searcher, imlist):
  """Returns the average number of correct images on the top four query results.
  """
  import numpy
  im_count = len(imlist)
  pos = numpy.zeros((im_count, 4))
  for i in range(im_count):
    # query() results are 1-based, convert to 0-based.
    pos[i] = [w[1] - 1 for w in searcher.query(imlist[i])[:4]]
  score = numpy.array([(pos[i] // 4) == (i // 4) for i in range(im_count)])*1.0
  return numpy.sum(score) / im_count


def plot_results(searcher, res):
  """Show images in result list `res`, a list of image ids."""
  import matplotlib.pyplot
  import numpy
  from PIL import Image
  matplotlib.pyplot.figure()
  res_count = len(res)
  for i in range(res_count):
    imname = searcher.get_filename(res[i])
    matplotlib.pyplot.subplot(1, res_count, i + 1)
    matplotlib.pyplot.imshow(numpy.array(Image.open(imname)))
    matplotlib.pyplot.axis('off')
  matplotlib.pyplot.show()
