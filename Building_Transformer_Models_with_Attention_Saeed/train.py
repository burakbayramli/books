import pickle
import os
from time import time

import tensorflow as tf
from tensorflow import data, math, reduce_sum, cast, equal, argmax, \
    float32, GradientTape, function
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.optimizers.schedules import LearningRateSchedule
from tensorflow.keras.metrics import Mean
from tensorflow.keras.losses import sparse_categorical_crossentropy

from transformer import TransformerModel
from prepare_dataset import PrepareDataset

print("tf version", tf.__version__)

BASE = "/opt/Downloads/nmt"
os.makedirs(BASE, exist_ok=True)
WEIGHTS_PATH = f"{BASE}/nmt.weights.h5"     # Keras 3 requires this extension

# ---------------- Parameters ----------------
h, d_k, d_v, d_model, d_ff, n = 8, 64, 64, 512, 2048, 6
EPOCHS = 20            # smoke test first; set to 20 for the real run
batch_size = 64
beta_1, beta_2, epsilon = 0.9, 0.98, 1e-9
dropout_rate = 0.1


class LRScheduler(LearningRateSchedule):
    def __init__(self, d_model, warmup_steps=4000, **kwargs):
        super().__init__(**kwargs)
        self.d_model = cast(d_model, float32)
        self.warmup_steps = warmup_steps

    def __call__(self, step_num):
        arg1 = step_num ** -0.5
        arg2 = step_num * (self.warmup_steps ** -1.5)
        return (self.d_model ** -0.5) * math.minimum(arg1, arg2)


optimizer = Adam(LRScheduler(d_model), beta_1, beta_2, epsilon)

# ---------------- Data ----------------
dataset = PrepareDataset()
(trainX, trainY, train_orig, enc_seq_length, dec_seq_length,
 enc_vocab_size, dec_vocab_size, enc_tokenizer, dec_tokenizer) = dataset(
    f"{BASE}/english-german-both.pkl")

# Save tokenizers/config and the actual training pairs NOW, so a crash
# during training doesn't lose them and inference can sanity-test on them.
with open(f"{BASE}/tokenizer_config.pkl", "wb") as f:
    pickle.dump({
        "enc_tokenizer": enc_tokenizer,
        "dec_tokenizer": dec_tokenizer,
        "enc_seq_length": enc_seq_length,
        "dec_seq_length": dec_seq_length,
        "enc_vocab_size": enc_vocab_size,
        "dec_vocab_size": dec_vocab_size,
    }, f)
with open(f"{BASE}/train_pairs.pkl", "wb") as f:
    pickle.dump(train_orig, f)

train_dataset = data.Dataset.from_tensor_slices((trainX, trainY)).batch(batch_size)

# ---------------- Model ----------------
training_model = TransformerModel(
    enc_vocab_size, dec_vocab_size, enc_seq_length,
    dec_seq_length, h, d_k, d_v, d_model, d_ff, n, dropout_rate
)


def loss_fcn(target, prediction):
    mask = cast(math.logical_not(equal(target, 0)), float32)
    loss = sparse_categorical_crossentropy(target, prediction, from_logits=True) * mask
    return reduce_sum(loss) / reduce_sum(mask)


def accuracy_fcn(target, prediction):
    mask = math.logical_not(equal(target, 0))
    acc = math.logical_and(mask, equal(target, argmax(prediction, axis=2)))
    return reduce_sum(cast(acc, float32)) / reduce_sum(cast(mask, float32))


train_loss = Mean(name="train_loss")
train_accuracy = Mean(name="train_accuracy")


@function
def train_step(encoder_input, decoder_input, decoder_output):
    with GradientTape() as tape:
        prediction = training_model(encoder_input, decoder_input, training=True)
        loss = loss_fcn(decoder_output, prediction)
        accuracy = accuracy_fcn(decoder_output, prediction)
    gradients = tape.gradient(loss, training_model.trainable_weights)
    optimizer.apply_gradients(zip(gradients, training_model.trainable_weights))
    train_loss(loss)
    train_accuracy(accuracy)


# ---------------- Training loop ----------------
for epoch in range(EPOCHS):
    train_loss.reset_state()
    train_accuracy.reset_state()
    print("\nStart of epoch %d" % (epoch + 1))
    start_time = time()

    for step, (batchX, batchY) in enumerate(train_dataset):
        encoder_input = batchX[:, 1:]       # drop <START> on the encoder side
        decoder_input = batchY[:, :-1]
        decoder_output = batchY[:, 1:]
        train_step(encoder_input, decoder_input, decoder_output)

        if step % 50 == 0:
            print(f"Epoch {epoch + 1} Step {step} Loss {train_loss.result():.4f} "
                  f"Accuracy {train_accuracy.result():.4f}")

    print(f"Epoch {epoch + 1}: Training Loss {train_loss.result():.4f}, "
          f"Training Accuracy {train_accuracy.result():.4f}")
    print("Total time taken: %.2fs" % (time() - start_time))

    if (epoch + 1) % 5 == 0 or epoch == EPOCHS - 1:
        training_model.save_weights(WEIGHTS_PATH)
        print("Saved weights at epoch", epoch + 1)

print("n trainable weights:", len(training_model.trainable_weights))
