import pickle
import numpy as np
import tensorflow as tf
from tensorflow.keras.preprocessing.sequence import pad_sequences
from transformer import TransformerModel

BASE = "/opt/Downloads/nmt"
WEIGHTS_PATH = f"{BASE}/nmt.weights.h5"

# ---------------- Load config ----------------
with open(f"{BASE}/tokenizer_config.pkl", "rb") as f:
    config = pickle.load(f)

enc_tokenizer  = config["enc_tokenizer"]
dec_tokenizer  = config["dec_tokenizer"]
enc_seq_length = config["enc_seq_length"]
dec_seq_length = config["dec_seq_length"]
enc_vocab_size = config["enc_vocab_size"]
dec_vocab_size = config["dec_vocab_size"]

h, d_k, d_v, d_model, d_ff, n = 8, 64, 64, 512, 2048, 6   # must match training
dropout_rate = 0.1

# ---------------- Build, then load ----------------
model = TransformerModel(
    enc_vocab_size, dec_vocab_size, enc_seq_length,
    dec_seq_length, h, d_k, d_v, d_model, d_ff, n, dropout_rate
)
# Training feeds the encoder enc_seq_length - 1 tokens; build with that shape.
model(tf.zeros((1, enc_seq_length - 1), dtype=tf.int64),
      tf.zeros((1, 1), dtype=tf.int64), training=False)
print(len(model.trainable_weights), "trainable weights in model")

before = [w.numpy().copy() for w in model.trainable_weights]
model.load_weights(WEIGHTS_PATH)
changed = sum(not np.allclose(b, w.numpy())
              for b, w in zip(before, model.trainable_weights))
print(f"{changed} / {len(before)} trainable weights changed after load")
if changed < len(before) * 0.9:
    raise SystemExit("Weights did not load properly, stopping.")

# ---------------- Translation ----------------
# Tokenizer strips '<' and '>' so the keys are 'start' / 'eos'
start_token = dec_tokenizer.word_index["start"]
end_token   = dec_tokenizer.word_index["eos"]


def translate(sentence, verbose=True):
    sentence = sentence.lower().strip()
    if verbose:
        oov = [w for w in sentence.split() if w not in enc_tokenizer.word_index]
        if oov:
            print(f"  [warning] not in encoder vocab (dropped): {oov}")

    seq = enc_tokenizer.texts_to_sequences(["<START> " + sentence + " <EOS>"])
    seq = pad_sequences(seq, maxlen=enc_seq_length, padding="post")
    encoder_input = tf.convert_to_tensor(seq[:, 1:], dtype=tf.int64)  # drop <START>

    decoder_input = tf.constant([[start_token]], dtype=tf.int64)
    out = []
    for _ in range(dec_seq_length - 1):
        preds = model(encoder_input, decoder_input, training=False)
        pid = int(tf.argmax(preds[:, -1, :], axis=-1).numpy()[0])
        if pid == end_token:
            break
        out.append(pid)
        decoder_input = tf.concat(
            [decoder_input, tf.constant([[pid]], dtype=tf.int64)], axis=-1)

    return " ".join(dec_tokenizer.index_word.get(i, "")
                    for i in out if i not in (0, start_token, end_token))

# ---------------- Custom tests ----------------
print("--- Custom tests ---")
for s in ["i am not a berliner sausage", "i am going to school"]:
    print(f"English: {s}\nGerman:  {translate(s)}\n")
