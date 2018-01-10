from collections import defaultdict, Counter
import numpy as np


class Ngram:
    """Generic ngram language model class.

    Parameters
    ----------
    n : int
        Number of tokens.

    """

    def __init__(self, n=2):
        self.n = n
        self.vocab_size = 0
        self.lm = None

    def ingest(self, orderedTokens):
        """Ingest data into language model.

        Parameters
        ----------
        orderedTokens: list of str
            Corpus data to be ingested to LM

        Returns
        -------
        self
        """
        self.vocab_size = len(orderedTokens)
        d = defaultdict(Counter)
        for i, word in enumerate(orderedTokens):
            if i == len(orderedTokens) - self.n:
                break
            currTup = tuple(orderedTokens[i:i + self.n - 1])
            d[currTup][orderedTokens[i + self.n - 1]] += 1
        self.lm = d
        return self

    def get_lm(self):
        return self.lm

    def get_candidates(self, token):
        return self.lm[token]

    def get_cond_prob(self, tup):
        context = tup[0:self.n - 1]
        target = tup[self.n - 1]
        num = self.lm[context][target]
        denom = np.sum([d[1] for d in self.lm[context].items()])
        return num / denom if denom != 0 else 0.0

    def get_cond_count(self, tup):
        context = tup[0:self.n - 1]
        target = tup[self.n - 1]
        num = self.lm[context][target]
        return num
