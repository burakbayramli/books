
# train/audio/_background_noise_/doing_the_dishes.wav
# train/audio/_background_noise_/dude_miaowing.wav
# train/audio/_background_noise_/exercise_bike.wav
# train/audio/_background_noise_/pink_noise.wav
# train/audio/_background_noise_/running_tap.wav
# train/audio/_background_noise_/white_noise.wav

import sounddevice as sd
import scipy.io.wavfile, zipfile
import io, time
zip = '/home/burak/Downloads/goog_voice_test.zip'
#zip = '/home/burak/Downloads/goog_voice_train.zip'
with zipfile.ZipFile(zip, 'r') as z:
     wav = io.BytesIO(z.open('test/audio/clip_e0ff06d6f.wav').read())
     v = scipy.io.wavfile.read(wav)
     fs = 16000
     scipy.io.wavfile.write('/tmp/tmp1.wav', fs, v[1])
     #sd.play(v[1], fs)
     #time.sleep(10)
     print v[1]

