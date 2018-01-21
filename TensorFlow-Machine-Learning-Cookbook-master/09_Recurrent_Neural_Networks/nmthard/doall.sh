python -u split_vocab.py
sort -u /tmp/vocab.en > /home/burak/Downloads/tur-eng/vocab.en
sort -u /tmp/vocab.tr > /home/burak/Downloads/tur-eng/vocab.tr
python -u dev_test.py
