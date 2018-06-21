# Dan + Jeff Do Data Science

Drafts for forthcoming data science book.

# Structure

Notes on what's in the repo:

- `\_book` is the book in its current form, knitted from the Rmd files in the root directory. This folder is self-contained.
- `\_bookdown_files` contains assets that are produced as a byproduct of knitting. 
- `\assets` contains images and data snippets used for light weight graphics in the book chapters. Most of the heavier data sets are stored on AWS and can be pulled using a wrapper that is in development (see [digIt](https://github.com/SigmaMonstR/digIt) -- needs to be formalized, but has 20+ example datasets)
- Strewn throughout the root directory are .Rmd files that contain the raw written material and code among other files that are necessary for Bookdown to work its magic