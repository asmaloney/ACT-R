[![GitHub release (latest by date)](https://img.shields.io/github/v/release/asmaloney/ACT-R)](https://github.com/asmaloney/ACT-R/releases/latest) [![GitHub](https://img.shields.io/github/license/asmaloney/ACT-R)](docs/LGPL.txt)

# ACT-R

This is a mirror of the official ACT-R distribution found [here](http://act-r.psy.cmu.edu/software/).

I created this mirror so that I could provide smaller ACT-R packages for use with my **[gactar project](https://github.com/asmaloney/gactar)**.

The only changes I made are adding this README file and copying the license to [LICENSE.txt](LICENSE.txt) so GitHub would pick it up.

[Releases](https://github.com/asmaloney/ACT-R/releases) are [tagged](https://github.com/asmaloney/ACT-R/tags) with their version number.

Each release creates the following files:
| File | Description |
|--|--|
| actr-slim-v7.x.y.zip | does not include examples, tutorials, and extras |
| actr-super-slim-v7.x.y.zip | does not include examples, tutorials, extras, docs, and most of the environment |
| Source code (zip) | full source in zip format |
| Source code (tar.gz) | full source in tar.gz format |

If you find a problem with the packaging of a release here, please [submit an issue](https://github.com/asmaloney/ACT-R/issues).

If you have issues with ACT-R itself, please [contact the ACT-R people](http://act-r.psy.cmu.edu/software/).

## License

ACT-R is licensed under the terms of the [LGPL 2.1](docs/LGPL.txt) license ([summary for non-lawyers](https://tldrlegal.com/license/gnu-lesser-general-public-license-v2.1-%28lgpl-2.1%29)).

## Original readme.txt

This is the ACT-R 7 source code distribution.

These directories contain all of the Lisp code needed to run ACT-R 7 and
also include a tutorial introduction to using ACT-R, a GUI for inspecting 
and debugging models called the ACT-R Environment, software documentation 
for ACT-R and the Environment, optional extensions which have been written 
for ACT-R, and also some example code showing ways of extending the system
and connecting other programming languages to the ACT-R remote interface.

To use the ACT-R Environment or connect ACT-R to other programming languages
requires that QuickLisp be installed for the Lisp being used to get the
libraries for network communication and threading.  QuickLisp isa available
from: <https://www.quicklisp.org>.  If the external connections are not
needed, ACT-R can also be run in a "Lisp only" mode without the need for
QuickLisp to be installed.  The "Lisp only" mode is typically much faster
at running models than the full system that allows for external connections. 

Instructions on how to load and run ACT-R and the ACT-R Environment can be
found in the QuickStart.txt file in the docs directory.  The tutorial units
are located in the tutorial directory, and that is the recommended starting 
point for those new to ACT-R.  More detailed information about the software 
can be found in the reference manual and the Environment manual which are 
located in the docs directory.

For more information or to download the most recent version of the ACT-R 
software please visit the ACT-R website at: <http://act-r.psy.cmu.edu>.
