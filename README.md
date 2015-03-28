Match by Keyword
========================================================

Matches a set of phrases in one column to its closest corresponding phrase in another column, utilizing the cosine similarity based on the tf - idf statistic. Note that multiple elements in "variation" may be mapped to the same element in "x". For a unique pairing, set one.to.one = TRUE.

## Installation
***
Since kwmatch is still under development it is not yet on CRAN, please download the development version. You can get the latest version from github with:

```{r}
install.packages("devtools")
library(devtools)
```

And then run the install_github command:

```{r}
install_github("kwmatch", "beckymaust")
library(kwmatch)
```

## Examples
***
Included in the package is a list of titles and abstracts for papers in the Open Journal of Statistics (http://www.scirp.org/journal/ojs/). Only the first sentence of the abstract is included for brevity. We match up the abstract to the corresponding title. We know that each title should correspond to a single abstract, so we set one.to.one = TRUE.


```{r}
set.seed(100)
journal <- load(kwmatch.journal)
keywordMatch(journal, journal[sample(nrow(journal)),2], one.to.one = TRUE)
```


