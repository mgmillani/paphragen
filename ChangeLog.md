# Revision history for paphragen

## 0.1.0.0  -- 2016-12-30
## 0.2.0.0  -- 2016-12-30

* First version.
    - A dictionary can be built based on text files.
    - Passphrases with desired entropy are generated.
    - Uses a stream of random bytes for generation.
* TODO
    - Check performance of IO operations.
    - Generate a non-ambiguous dictionary, that is, guarantee that there is only one way of generating each passphrase. For example, if `in`, `put` and `input` are in the dictionary, the passphrase `input` is ambiguous. In this case, either `in` or `put` should be removed.
