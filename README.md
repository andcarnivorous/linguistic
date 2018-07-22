# Linguistic Mode
A very simple Emacs Minor Mode that allows to perform basic linguistic / text analysis on a buffer or region contents.

## How to Install it

By using `package-build-create-recipe` you can install the package like this:

```elisp
(linguistic :fetcher github :repo "andcarnivorous/linguistic-mode" :files ("*.el" "*.org"))

```

Then load the file and activate the mode with `M-x` `linguistic-mode`

## How the linguistic-mode works

#### Collocation

The function `linguistic-collocation` allows to search for all the occurrences of a word and show the context in which it is used in the buffer. The context can be any number of words on the left and right, chosen by the user. This gives a basic functionality similar to what software like Antconc provide.

Example (Keyword: woodchuck, context: 2 words left, 2 words right.):

How much wood would a woodchuck chuck if a woodchuck would chuck wood.

```
(would a)  WOODCHUCK  (chuck if) 
(if a)  WOODCHUCK  (would chuck)
```

#### Word and Ngram Frequencies

The functions `linguistic-word-freq` and `linguistic-grams-freq` return an org buffer with a table of the most frequent words, their occurrence and ready-to-use snippets in Python, gnuplot and R to plot the results. The functions also save a CSV copy of the org-table in the home directory.

You can also choose whether or not to include `linguistic-stopwords` in the results and can change the list of stopwords with `M-x` `customize-variable` `linguistic-stopwords`.

![example](img/wordfreq.png)

## New Features TODO list:

- [ ] Customizable ngrams

- [ ] Snowball stemmer
