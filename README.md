# MMCK CHICKEN Docs

[![Build Status](https://travis-ci.org/marcomaggi/mmck-chicken-docs.svg?branch=master)](https://travis-ci.org/marcomaggi/mmck-chicken-docs)

## Introduction

This project gathers unofficial documentation about the CHICKEN compiler
and  its development  environment.   CHICKEN is  a Scheme-to-C  compiler
supporting the language features as defined in the ``Revised^5 Report on
Scheme''.

The  package targets  POSIX systems.   To  run the  tests: this  package
depends upon the package MMCK Checks.

The package uses the GNU Autotools and it is tested, using Travis CI, on
both Ubuntu GNU+Linux systems and OS X systems.

The last time  the maintainer bothered to update this  paragraph: he had
tested CHICKEN 5.0.0.

## License

See the individual documentation files for the copyright notices.

## Install

To install from a proper release tarball, do this:

```
$ cd mmck-chicken-docs-0.1.0
$ mkdir build
$ cd build
$ ../configure
$ make
$ make check
$ make install
```

to inspect the available configuration options:

```
$ ../configure --help
```

The Makefile is designed to allow parallel builds, so we can do:

```
$ make -j4 all && make -j4 check
```

which,  on  a  4-core  CPU,   should  speed  up  building  and  checking
significantly.

The Makefile supports the DESTDIR  environment variable to install files
in a temporary location, example: to see what will happen:

```
$ make -n install DESTDIR=/tmp/mmck-chicken-docs
```

to really do it:

```
$ make install DESTDIR=/tmp/mmck-chicken-docs
```

After the  installation it is  possible to verify the  installed library
against the test suite with:

```
$ make installcheck
```

From a repository checkout or snapshot  (the ones from the Github site):
we  must install  the GNU  Autotools  (GNU Automake,  GNU Autoconf,  GNU
Libtool), then  we must first run  the script `autogen.sh` from  the top
source directory, to generate the needed files:

```
$ cd mmck-chicken-docs
$ sh autogen.sh

```

After this  the procedure  is the same  as the one  for building  from a
proper release tarball, but we have to enable maintainer mode:

```
$ ../configure --enable-maintainer-mode [options]
$ make
$ make check
$ make install
```

When compiling  the environment  variable CHICKEN_FLAGS is  available to
hand options to the compiler:

```
$ make CHICKEN_FLAGS='-d3'
```

## Usage

Read the documentation generated from  the Texinfo sources.  The package
installs the documentation  in Info format; we can  generate and install
documentation in HTML format by running:

```
$ make html
$ make install-html
```

## Credits

The documentation was  gathered, and sometimes written,  by Marco Maggi.
If this package exists it's because of the great GNU software tools that
he uses  all the time.   CHICKEN was originally  a creation of  Felix L.
Winkelmann, it is now developed and maintained The CHICKEN Team.

See the individual documentation files for the list of authors.

## Bugs, vulnerabilities and contributions

Bug  and vulnerability  reports are  appreciated, all  the vulnerability
reports  are  public; register  them  using  the  Issue Tracker  at  the
project's GitHub  site.  For  contributions and  patches please  use the
Pull Requests feature at the project's GitHub site.

## Resources

The latest release of this package can be downloaded from:

[https://bitbucket.org/marcomaggi/mmck-chicken-docs/downloads](https://bitbucket.org/marcomaggi/mmck-chicken-docs/downloads)

development takes place at:

[http://github.com/marcomaggi/mmck-chicken-docs/](http://github.com/marcomaggi/mmck-chicken-docs/)

and as backup at:

[https://bitbucket.org/marcomaggi/mmck-chicken-docs/](https://bitbucket.org/marcomaggi/mmck-chicken-docs/)

the documentation is available online:

[http://marcomaggi.github.io/docs/mmck-chicken-docs.html](http://marcomaggi.github.io/docs/mmck-chicken-docs.html)

the GNU Project software can be found here:

[http://www.gnu.org/](http://www.gnu.org/)

we can download CHICKEN from:

[http://www.call-cc.org/](http://www.call-cc.org/)

the package MMCK Checks is available from:

[https://github.com/marcomaggi/mmck-checks/](https://github.com/marcomaggi/mmck-checks/)

## Badges and static analysis

### Travis CI

Travis CI is  a hosted, distributed continuous  integration service used
to build and test software projects  hosted at GitHub.  We can find this
project's dashboard at:

[https://travis-ci.org/marcomaggi/mmck-chicken-docs](https://travis-ci.org/marcomaggi/mmck-chicken-docs)

Usage of this  service is configured through the  file `.travis.yml` and
additional scripts are under the directory `meta/travis-ci`.

