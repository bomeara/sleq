[![Build Status](https://travis-ci.org/bomeara/sleq.svg)](https://travis-ci.org/bomeara/sleq)

# sleq
A sleek package to deal with sequences in R. Joint O'Meara lab project, created in a hackathon.

To install it:
```
library(devtools)
install_github(bomeara/sleq)
```

## Hackathon results
* One undergrad, five grad students, three postdocs, and a faculty member came up with issues over the course of a couple of lab meetings
* Before the hackathon, we created an example function and a new class for sequence alignments
* At the hackathon, we split into pairs and chose issues to work on
* We mostly kept to our goals of using unit testing and roxygen throughout. The main exception was some pre-existing code in this area from people's own work that was simply incorporated, with unit testing to be added later.
* [Ten lab members committed code to this project](https://github.com/bomeara/sleq/pulse); for many, it was the first time using version control software, issue tracking, and the like.
* The code now has:
** ```seqalignment``` class: can be subset with brackets like a character matrix, but internally has slots for info on codon position, gene name, and data type. Tested.
** Code to subset these objects in various ways.
** Code to merge these objects, even with partial taxon overlap: combine COI and 18S alignments, for example.
** Code to filter for areas of bad alignment.
** Code to automatically find the reading frame that will minimize stop codons. Tested.
** As well as other code that is less well tested, but still in development.

## Archived info from the day of the hackathon
This will be developed at an inhouse hackathon on Oct. 12, 2015. A secondary objective is to make useful code, but the primary objectives are learning and lab bonding. To that end:

* We will be using [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) for documentation
* We are doing unit testing with [testthat](https://github.com/hadley/testthat) -- intro [here](http://r-pkgs.had.co.nz/tests.html). In our case, we will write tests for a function, which it will fail at first, then we will write code to make it work.
* We will be doing [pair programming](https://en.wikipedia.org/wiki/Pair_programming): this results in better code, but more importantly helps the primary objectives of learning and bonding (unless the pairs fight, in which case it becomes learning that you're not bonding).
* We'll be using [Google's R style guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml).
* We are using a shared github repo (here!). Note that if you submit new code by doing pull requests, you can get a [free, geeky t-shirt](https://hacktoberfest.digitalocean.com/)
* Lab members: before we start, get a github account. I advise using a permanent email address (UT will kick you out after you leave).

Some notable features of the ```sleq``` package:
* Sequences are stored as character matrices, thus easy to subset.
* However, we also have a ```seqalignment``` class. It can be indexed using ```[]``` as with a character matrix, but actually stores this internally. The object also has info about gene locations and sequence type.
