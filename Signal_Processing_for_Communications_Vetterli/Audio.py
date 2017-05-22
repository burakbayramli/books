from IPython.core.display import DisplayObject

class Audio(DisplayObject):
    """Create an audio object.

    When this object is returned by an input cell or passed to the
    display function, it will result in Audio controls being displayed
    in the frontend (only works in the notebook).
    
    Parameters
    ----------
    data : numpy array, unicode, str or bytes
        Can be a
        * Numpy array containing the desired waveform,
        * String containing the filename
        * Bytestring containing raw PCM data or
        * URL pointing to a file on the web. 
        
        If the array option is used the waveform will be normalized.
        
        If a filename or url is used the format support will be browser 
        dependent. 
    url : unicode
        A URL to download the data from.
    filename : unicode
        Path to a local file to load the data from.
    embed : boolean
        Should the image data be embedded using a data URI (True) or be
        loaded using an <img> tag. Set this to True if you want the image
        to be viewable later with no internet connection in the notebook.

        Default is `True`, unless the keyword argument `url` is set, then
        default value is `False`.
    rate : integer
        The sampling rate of the raw data.
        Only required when data parameter is being used as an array
    autoplay : bool
        Set to True if the audio should immediately start playing.
        Default is `False`.

    Examples
    --------
  
    # Generate a sound
    import numpy as np
    framerate = 44100
    t = np.linspace(0,5,framerate*5)
    data = np.sin(2*np.pi*440*np.sin(10*t**2))
    Audio(data,rate=framerate)

    Audio("http://www.nch.com.au/acm/8k16bitpcm.wav")
    Audio(url="http://media.bradsucks.net/albums/ooi-128/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3")
    Audio(url="http://www.w3schools.com/html/horse.ogg", embed=True)
    
    Audio('/path/to/sound.wav')
    Audio(filename='/path/to/sound.ogg')
    
    Audio(b'RAW_WAV_DATA..)
    Audio(data=b'RAW_WAV_DATA..)

    """
        
    def __init__(self, data=None, filename=None, url=None, embed=None, rate=None, autoplay=False):
        if filename is None and url is None and data is None:
            raise ValueError("No image data found. Expecting filename, url, or data.")
        if embed is False and url is None:
            raise ValueError("No url found. Expecting url when embed=False")
            
        if url is not None and embed is not True:
            self.embed = False
        else:
            self.embed = True
        self.autoplay = autoplay
        super(Audio, self).__init__(data=data, url=url, filename=filename)
            
        if self.data is not None and not isinstance(self.data, bytes):
            self.data = self._make_wav(data,rate)
            
    def reload(self):
        """Reload the raw data from file or URL."""
        import mimetypes
        if self.embed:
            super(Audio, self).reload()

        if self.filename is not None:
            self.mimetype = mimetypes.guess_type(self.filename)[0]
        elif self.url is not None:
            self.mimetype = mimetypes.guess_type(self.url)[0]
        else:
            self.mimetype = "audio/wav"
            
            
    def _make_wav(self,data,rate):
        """ Transform a numpy array to a PCM bytestring """
        import struct
        from io import BytesIO
        import wave
        maxabsvalue = max(map(abs,data))
        scaled = map(lambda x: int(x/maxabsvalue*32767), data)
        fp = BytesIO()
        waveobj = wave.open(fp,mode='wb')
        waveobj.setnchannels(1)
        waveobj.setframerate(rate)
        waveobj.setsampwidth(2)
        waveobj.setcomptype('NONE','NONE')
        waveobj.writeframes(b''.join([struct.pack('<h',x) for x in scaled]))
        val = fp.getvalue()
        waveobj.close()
        return val
    
    def _data_and_metadata(self):
        """shortcut for returning metadata with url information, if defined"""
        md = {}
        if self.url:
            md['url'] = self.url
        if md:
            return self.data, md
        else:
            return self.data
        
    def _repr_html_(self):
        src = """
                <audio controls="controls" {autoplay}>
                    <source src="{src}" type="{type}" />
                    Your browser does not support the audio element.
                </audio>
              """
        return src.format(src=self.src_attr(),type=self.mimetype, autoplay=self.autoplay_attr())

    def src_attr(self):
        import base64
        if self.embed and (self.data is not None):
                return """data:{type};base64,{base64}""".format(type=self.mimetype, 
                                                                base64=base64.b64encode(self.data).decode('ascii'))
        elif self.url is not None:
            return self.url
        else:
            return ""

    def autoplay_attr(self):
        if(self.autoplay):
            return 'autoplay="autoplay"'
        else:
            return ''