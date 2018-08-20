# Style guide

This document sets up a style guide for our book. We can also use it for the a style guide for the readers. (Meta.) They will not need the _Language_ section.

## Language

### General language

- When referencing R, do not use monospace or quotes or any other styling—just type R.
- When entering commands into R's console, use "execute `command` in your/the R console".
- Distinguish between _console_ and _command line_.
- When referencing an R function, use the monospaced name, e.g., "The function `foo` is very generic."
- "e.g.," (or "E.g.," at the start of a sentence) and "i.e.," (or "I.e.," at a start of a sentence):
  - Are not italicized.
  - Always include a comma after the second period.
  - "E.g.," is used in exchange for "for example".
  - "I.e.," is used in exchange for "specifically".

### Flagging

- "TODO" flags a item that needs to be finished (and possibly started).
- "FIXME" flags a topic that needs to be (wait for it...) fixed.

## Our style guide

### Naming conventions

#### Naming files

If the file is an R script, then its suffix should be `.R`. The file's name should tell you (and other people) what the file (script) contains (does).

No spaces in file names. You'll be sorry if you do. If you need multiple words, then use camelcase, starting with a lowercase (_e.g._, thisIsCamelcase).

```{R}
# Good
analyzeArrests.R
mapFires.R
# Bad
regression.r
Make A Map.R
```

If you have scripts that run in succession, it is often helpful to start the scripts' names with numbers.

#### Naming objects

Object names are lowercase, descriptive, succinct, and separated by underscores (`_`) when needed. Nouns are good. Numbers work. Bonus points for including type of object.

```{R}
# Great
arrest_df
# Good
arrests
# Not so good
arrest_data
# Bad
some.data
someArrestDataIFoundOnline
# Terrible
d
```

_Note:_ These rules are general guidelines. In some cases in may be reasonable to name an object `d`.

## Other style guides

- [Hadley Wickham's _Advanced R_ style guide][s0]
- The [`tidyverse` style guide][s1]
- [Google's R Style Guide][s2]
- [R Style Guide from JEFworks][s3]

[s0]: http://adv-r.had.co.nz/Style.html
[s1]: http://style.tidyverse.org
[s2]: https://google.github.io/styleguide/Rguide.xml
[s3]: http://jef.works/R-style-guide/
