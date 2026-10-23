from tensorflow import data, train, math, reduce_sum, cast, equal, argmax, \
    float32, GradientTape, function, linalg, ones, maximum, newaxis
from tensorflow.keras import Model
from tensorflow.keras.layers import Dense
from tensorflow.keras.losses import sparse_categorical_crossentropy

# External imports required by TransformerModel
from encoder import Encoder
from decoder import Decoder

# --- Transformer Model (from Listing 19.9) ---
class TransformerModel(Model):
    def __init__(self, enc_vocab_size, dec_vocab_size, enc_seq_length, dec_seq_length,
                 h, d_k, d_v, d_model, d_ff_inner, n, rate, **kwargs):
        super().__init__(**kwargs)
        # Set up the encoder
        self.encoder = Encoder(enc_vocab_size, enc_seq_length, h, d_k, d_v,
                               d_model, d_ff_inner, n, rate)
        # Set up the decoder
        self.decoder = Decoder(dec_vocab_size, dec_seq_length, h, d_k, d_v,
                               d_model, d_ff_inner, n, rate)
        # Define the final dense layer
        self.model_last_layer = Dense(dec_vocab_size)

    def padding_mask(self, input):
        # Create mask which marks the zero padding values in the input by a 1.0
        mask = math.equal(input, 0)
        mask = cast(mask, float32)

        # The shape of the mask should be broadcastable to the shape
        # of the attention weights that it will be masking later on
        return mask[:, newaxis, newaxis, :]

    def lookahead_mask(self, shape):
        # Mask out future entries by marking them with a 1.0
        mask = 1 - linalg.band_part(ones((shape, shape)), -1, 0)
        return mask

    def call(self, encoder_input, decoder_input, training):
        # Create padding mask to mask the encoder inputs and the encoder
        # outputs in the decoder
        enc_padding_mask = self.padding_mask(encoder_input)

        # Create and combine padding and look-ahead masks to be fed into the decoder
        dec_in_padding_mask = self.padding_mask(decoder_input)
        dec_in_lookahead_mask = self.lookahead_mask(decoder_input.shape[1])
        dec_in_lookahead_mask = maximum(dec_in_padding_mask, dec_in_lookahead_mask)

        # Feed the input into the encoder
        encoder_output = self.encoder(encoder_input, enc_padding_mask, training=training)

        # Feed the encoder output into the decoder
        decoder_output = self.decoder(decoder_input, encoder_output,
                                      dec_in_lookahead_mask, enc_padding_mask, training=training)

        # Pass the decoder output through a final dense layer
        model_output = self.model_last_layer(decoder_output)
        return model_output

