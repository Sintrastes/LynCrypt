LynCrypt
========

LynCyrpt is both a Haskell library and an executable, which implements a variant of the Asmuth-Bloom cryptosystem; a [secret sharing scheme](http://en.wikipedia.org/wiki/Secret_sharing) based on the [Chinese remainder theorem](http://en.wikipedia.org/wiki/Chinese_remainder_theorem).

Installation
============

LynCrypt is distributed as a cabal package, so if you already have a recent version of the Haskell platform installed, simply clone and cd into this repo, then `cabal install`. 

Usage
=====

The first two arguments to the lyncrypt binary are always `n` and `k`, respectively, where `n` is the number of shares generated, and `k` is the number of shares required to recover the secret (meaning 1 <= k <= n).

In order to encrypt a file, we specify a file name of the message to encrypt with the `--encrypt` flag, then a list of output file names, one "m0" (which is always the first argument to the `--output` flag) and `n` shares.

    lyncrypt n k --encrypt message --output m0 file_1 ... file_n

The "m0" is an artifact of the Asmuth-Bloom cryptosystem, if you really only want a scheme with n shares where the secret can only be recovered if and only if k or more shares are present, you can distribute the m0 file amongst all of the share-holders. However, you could also treat the m0 as it's own independent share, which makes for a security scheme in which to recover the secret, at least `k` of the `n` shares, *and* the m0 key are needed to recover the secret. With such a scheme, the holder of m0 can be thought of intuitively as the "gatekeeper" of the secret -- even if all of the other n shares have gathered to recover the secret, they still cannot do it without the m0 key, regardless of the threshold value k chosen.

To decrypt, simply call lyncrypt with the `--decrypt` flag, with m_0 as the first argument, and the keys as the other arguments, and pass the name of the file you would like to output the secret to as an argument to the `--output` flag.

    lyncrypt n k --decrypt m0 file_1 ... file_n --output output_file


*** Special Flags

By default, LynCrypt uses a binary serialization format as output, this can be changed to a more human-readable format by passing the `--readable` flag.

Feature Ideas
=============

  * Function sharing support, (possibly as described in [this paper](http://www.cs.bilkent.edu.tr/~selcuk/publications/AB_ISCIS06.pdf).
  * Support for [Mignotte's threshold secret sharing scheme](http://en.wikipedia.org/wiki/Secret_sharing_using_the_Chinese_remainder_theorem#Mignotte.27s_threshold_secret_sharing_scheme)
  * Support for other secret sharing systems.
    
Disclaimer
========

This is experimental software in a very early stage of development, and has not been formally verified, nor audited by security professionals -- use at your own risk, see LICENSE for full disclaimer and copyright information.

Also keep in mind that LynCrypt does not yet support function sharing, meaning that the secret sharing it implements is really not much more than a novelty at this point, because the shares muse be computed in one central place (function sharing is not yet supported), so whoever computes and distributes the shares will have full access to the secret. However, this still could be useful for adding an interesting element to a cryptography based challenge, which is part of the reason I created LynCrypt.

Important Note (bug)
====================
Currently when using some k combinations of the n shares to recover the secret, sometimes the secret is not fully recovered, so the full threshold secret sharing scheme doesn't work perfectly yet, for unclear reasons. 

