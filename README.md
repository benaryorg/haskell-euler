# What's this?

Well, [project euler](https://projecteuler.net/) again, this time not in Rust
(didn't even make that public back then) but in Haskell.

# Dependencies?

Yes, there are three so far:

- split: necessary for overlapping splitting
- arithmoi: I had my own prime number generator but it was too slow and I can't
  use [my rust version](https://github.com/benaryorg/rust-prime_iter) so I
  stick to that one
- memoize: memoizing, i.e. pure function caching; stuff you'd use Maps in
  non-pure functional languages for

