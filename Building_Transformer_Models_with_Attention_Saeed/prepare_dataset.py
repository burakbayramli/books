from pickle import load
from numpy.random import seed
from numpy.random import shuffle
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
from tensorflow import convert_to_tensor, int64

class PrepareDataset:
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.n_sentences = 10000  # Number of sentences to include in the dataset
        self.train_split = 0.9    # Ratio of the training data split

    # Fit a tokenizer
    def create_tokenizer(self, dataset):
        tokenizer = Tokenizer()
        tokenizer.fit_on_texts(dataset)
        return tokenizer

    def find_seq_length(self, dataset):
        return max(len(seq.split()) for seq in dataset)

    def find_vocab_size(self, tokenizer, dataset):
        tokenizer.fit_on_texts(dataset)
        return len(tokenizer.word_index) + 1

    def __call__(self, filename, **kwargs):
        # Load a clean dataset
        clean_dataset = load(open(filename, 'rb'))

        # Reduce dataset size
        dataset = clean_dataset[:self.n_sentences, :]

        # Include start and end of string tokens
        for i in range(dataset[:, 0].size):
            dataset[i, 0] = "<START> " + dataset[i, 0] + " <EOS>"
            dataset[i, 1] = "<START> " + dataset[i, 1] + " <EOS>"

        # Random shuffle the dataset
        shuffle(dataset)

        # Split the dataset
        train = dataset[:int(self.n_sentences * self.train_split)]

        # Prepare tokenizer for the encoder input
        enc_tokenizer = self.create_tokenizer(train[:, 0])
        enc_seq_length = self.find_seq_length(train[:, 0])
        enc_vocab_size = self.find_vocab_size(enc_tokenizer, train[:, 0])

        # Encode and pad the input sequences
        trainX = enc_tokenizer.texts_to_sequences(train[:, 0])
        trainX = pad_sequences(trainX, maxlen=enc_seq_length, padding='post')
        trainX = convert_to_tensor(trainX, dtype=int64)

        # Prepare tokenizer for the decoder input
        dec_tokenizer = self.create_tokenizer(train[:, 1])
        dec_seq_length = self.find_seq_length(train[:, 1])
        dec_vocab_size = self.find_vocab_size(dec_tokenizer, train[:, 1])

        # Encode and pad the input sequences
        trainY = dec_tokenizer.texts_to_sequences(train[:, 1])
        trainY = pad_sequences(trainY, maxlen=dec_seq_length, padding='post')
        trainY = convert_to_tensor(trainY, dtype=int64)
    
        return trainX, trainY, train, enc_seq_length, dec_seq_length, enc_vocab_size, dec_vocab_size, enc_tokenizer, dec_tokenizer
