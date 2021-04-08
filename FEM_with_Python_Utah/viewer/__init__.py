import os
from traits.etsconfig.api import ETSConfig
toolkit = os.getenv('ETS_TOOLKIT', 'qt4')
ETSConfig.toolkit = toolkit
os.environ['ETS_TOOLKIT'] = toolkit
from _viewer import launch_viewer
