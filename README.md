[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=ivanperez-keera&url=https://github.com/ivanperez-keera/hcwiid&title=HCwiid&language=&tags=github&category=software) 

This is a refurbished version of hcwiid. It will include most of cwiid,
and I've already began to include extra functions that I find useful.
If you find something in cwiid which is not included here and you need it,
please let me know/send a pull request.

Instructions
============

The official announcement, which describes the available features,
can be found here:
http://keera.co.uk/blog/2014/05/27/interacting-haskell-games-using-wiimote/

Installation
============

You can either run cabal update and install directly from hackage
(recommended), or clone this repository and install it with cabal. 

Compiling programs that use the wiimote (special flags)!
========================================================

How the frag do I compile the tests in test/?

You need to do two things:
1) Compile the program with -rtsopts (enable RTS command line flags)
2) Run the program with +RTS -V0
   This will disable the internal ticks used for profiling, which
   in turn will make the runtime system not use the SIGALRM signal,
   which libluetooth uses for its own internal purposes and is necessary
   to connect to the wiimote.

   (This bit is the work of Paolo Capriotti, whose mastery of GHC
    baffles me every time.)

Projects using this
===================
* Carettah (http://carettah.masterq.net/)

TODO
========
* Locate more projects using this. (I am personally working on some games that
  will be published.)
* Write a decent example that uses more than just testing a button thing.
