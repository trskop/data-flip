Flip
====

Data type that flipps last two of its type arguments.


Building options
----------------

* `-fwith-comonad` (enabled by default)
  Define various `Comonad` instances.
* `-fwith-deepseq` (enabled by default)
  Define `NFData` instance for `Flip`.
* `-fpedantic` (disabled by default)
  Pass additional warning flags to GHC.


Contributions
-------------

Pull requests, bug reports and generally contributions in any form are welcome!
Please don't be afraid to contact author using GitHub or by e-mail (see
`.cabal` file for that).

Also try to use `-fpedantic` flag during development and testing.


Lincense
--------

This package is under BSD3 license, see `LICENSE` file for details.

All dependencies are also under the same license, that includes optional
dependencies. See individual packages on hackage for details:

* [base][]
* [comonad][]
* [deepseq][]


[base]:
  http://hackage.haskell.org/package/base/
  "HackageDB: base"

[comonad]:
  http://hackage.haskell.org/package/comonad/
  "HackageDB: comonad"

[deepseq]:
  http://hackage.haskell.org/package/deepseq/
  "HackageDB: deepseq"
