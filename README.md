# paphragen - A passphrase generator

## Synopsis
A passphrase is a password made with words instead of just letters and digits. The advantage over passwords is that they are easier to remember. The disadvantage is that they are quite long and people tend to overestimate their security.

Most tools for password generation that we know just generate a random sequence of characters and cannot properly estimate the strength of a passphrase.

`paphragen` is capable not only of generating passphrases from a given word list, it can also build such word lists given sufficient text input (e.g. books, news articles...). It also properly computes the strength of the generated password. A stream of random bytes can be used in order to achieve cryptographic-quality randomness.

## Building

Just make sure you have `ghc` and `cabal` installed. Then run

    cabal install

The binary `paphragen` will be created.

## Usage

### Building a word list

To build a dictionary based on the words of input files, run:

    paphragen build [OPTIONS...] <TEXT_FILE...>


where `OPTIONS` are:


    -o, --output DICTIONARY     writes output to DICTIONARY instead of stdout.

Example:

    paphragen build -o dictionary book1.txt shopping.txt

It is recommended to use multiple megabytes of input text in order to properly cover enough words and to filter out rare ones. It is safe to use personally relevant texts here, since the strength of the passphrase depends on its length and on the size of the dictionary. Even if an attacker knows which word list you used, you passphrase should be secure. Generating your own list only makes the attacker also have to guess the word list.

If you want to guarantee that certain words are on the list, you can add them manually to another dictionary and inform both on generation.

### Generating a passphrase

To generate a password using an existing dictionary:

    paphragen generate [OPTIONS...] <DICTIONARY...>

where `OPTIONS` are:

    -e, --entropy N      sets the minimum desired entropy (default: 100 bits).
    -l, --length N       number of words to use (entropy is used by default).

In this case, a random sequence of bytes should be provided through `stdin`.
On Unix-like systems, `/dev/random` is a good choice.

Example:

    paphragen generate -e 90 diceware.txt shopping.txt animals.txt < /dev/random

Note that `< /dev/random` will read `/dev/random` and pass the contents to the `stdin` of `paphragen`.

A popular (though a bit outdated) word list is [diceware](https://en.wikipedia.org/wiki/Diceware). There are links to other lists on that Wikipedia article.
