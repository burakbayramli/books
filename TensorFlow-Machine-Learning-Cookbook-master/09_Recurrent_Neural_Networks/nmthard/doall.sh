python -u split_vocab.py
split -l 360000 /tmp/train.en /tmp/out1en
split -l 20000 /tmp/out1enab /tmp/out2en
mv /tmp/out1enaa /home/burak/Downloads/tur-eng/train.en
mv /tmp/out2enaa /home/burak/Downloads/tur-eng/tst2012.en
mv /tmp/out2enab /home/burak/Downloads/tur-eng/tst2013.en

split -l 360000 /tmp/train.tr /tmp/out1tr
split -l 20000 /tmp/out1trab /tmp/out2tr
mv /tmp/out1traa /home/burak/Downloads/tur-eng/train.tr
mv /tmp/out2traa /home/burak/Downloads/tur-eng/tst2012.tr
mv /tmp/out2trab /home/burak/Downloads/tur-eng/tst2013.tr
