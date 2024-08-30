# reside.utils

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/reside-ic/reside.utils/workflows/R-CMD-check/badge.svg)](https://github.com/reside-ic/reside.utils/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://codecov.io/github/mrc-ide/reside.utils/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/reside.utils?branch=main)
<!-- badges: end -->

## Usage

```r
usethis::use_standalone("reside-ic/reside.utils", "standalone-utils-assert.R")
```

To exclude this file from coverage counts, make a `.covrignore` file containing:

```
R/import-*.R
```

To exclude these files from GitHub diffs by default

```
R/import-*.R linguist-generated=true
```

## Installation

To install `reside.utils`:

```r
remotes::install_github("mrc-ide/reside.utils", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
