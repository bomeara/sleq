[![Build Status](https://travis-ci.org/bomeara/sleq.svg)](https://travis-ci.org/bomeara/sleq)

# sleq
A sleek package to deal with sequences in R. Joint O'Meara lab project.

This will be developed at an inhouse hackathon on Oct. 12, 2015. A secondary objective is to make useful code, but the primary objectives are learning and lab bonding. To that end:

* We will be using [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) for documentation
* We are doing unit testing with [testthat](https://github.com/hadley/testthat) -- intro [here](http://r-pkgs.had.co.nz/tests.html). In our case, we will write tests for a function, which it will fail at first, then we will write code to make it work.
* We will be doing [pair programming](https://en.wikipedia.org/wiki/Pair_programming): this results in better code, but more importantly helps the primary objectives of learning and bonding (unless the pairs fight, in which case it becomes learning that you're not bonding).
* We'll be using [Google's R style guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml).
* We may try using Gitter for communication during the hackathon (though we'll all be in the same room, it can be less distracting than shouting "Hey, you!"). Connect [here](https://gitter.im/bomeara/omearalabpublic?utm_source=share-link&utm_medium=link&utm_campaign=share-link). Note that it is **public**: this makes it easier to join, but make sure you don't say anything you don't want everyone to see.
* We are using a shared github repo (here!). Note that if you submit new code by doing pull requests, you can get a [free, geeky t-shirt](https://hacktoberfest.digitalocean.com/)
* Lab members: before we start, get a github account. I advise using a permanent email address (UT will kick you out after you leave).

Some notable features of the ```sleq``` package:
* Sequences are stored as character matrices, thus easy to subset.
* However, we also have a ```seqalignment``` class. It can be indexed using ```[]``` as with a character matrix, but actually stores this internally. The object also has info about gene locations and sequence type.
