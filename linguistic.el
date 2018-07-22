;;; linguistic.el --- A package for basic linguistic analysis.

;; Copyright (C) 2018 Andrew Favia

;; Author: Andrew Favia <drewlinguistics01 at gmail dot com>
;; Version: 0.1
;; Package-Requires ((cl-lib "0.5") (emacs "25"))
;; Keywords: linguistics, text analysis, matching
;; URL: https://github.com/andcarnivorous/linguistic

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; Carry out basic linguistic / text analysis on buffers with word frequency, bigrams, trigrams and collocation.
;;
;; Usage:
;; `linguistic-word-freq' and `linguistic-grams-freq' return frequencies ready to plot.
;; `linguistic-collocation' returns word occurrences in context.
;; 

;;; Code:

(require 'cl-lib)

(defgroup linguistic-analysis nil
  "Linguistic-mode settings for stopwords and extraction."
  :group 'linguistic-analysis)

(defcustom linguistic-splitter-characters "[\\*\\-\"\\.\\,\\:\\?\\!]" "Characters to find with RegEx for the function ‘linguistic-splitter’."
  :group 'linguistic-analysis
  :type 'string)

(defcustom linguistic-stopwords '("what" "am" "during" "him" "we" "me" "under" "the"
				  "i" "myself" "again" "be" "you" "yourself" "once" "that"
				  "my" "yourselves" "here" "your" "d" "there" "any"
				  "it" "ll" "when" "of" "he" "yours" "where" "did"
				  "she" "himself" "all" "so" "her" "herself" "same" "any"
				  "our" "this" "own" "some" "they" "those" "could" "but"
				  "their" "these" "can" "for" "on" "which" "won" "with"
				  "in" "who" "will" "not" "at" "when" "just" "no"
				  "or" "been" "now" "yes" "and" "is" "ve" "being"
				  "because" "are" "hadn" "as" "an" "have" "needn" "from"
				  "a" "has" "need" "by" "to" "had" "haven" "”"
				  "his" "was" "hasn" "—" "wasn" "don" "t" "“"
				  "do" "would" "isn" "does" "should")
  "List of stopwords to exclude from extraction."
  :group 'linguistic-analysis
  :type '(repeat string))

(defun linguistic-collocation ()
  "Search for and return every occurrence of a keyword in the buffer plus the words on its sides (as many as given on each side)."
  (interactive)
  (let* ((x 0)
	 (y 0)
	 (numafter (read-number "insert number of words after: "))
	 (numbefore (read-number "insert number of words before: "))
	 (words (split-string
                 (downcase (replace-regexp-in-string "[\.\,\:\?\!\"\-\;]" " . " (buffer-string)))))
	 (keyword (read-string "insert the word you are searching:"))
	 (onright '())
	 (onleft '()))
    (with-current-buffer (get-buffer-create "*collocation*")
      (while (member keyword words)
	(while (< x numafter)
	  (setq x (1+ x))
	  (add-to-list 'onright (nth (+ (seq-position words keyword) x) words)))
	(while (< y numbefore)
	  (setq y (1+ y))
	  (add-to-list 'onleft (nth (- (seq-position words keyword) y) words)))
	(progn
	  (insert " \n" (concat (format "%s" onleft) "  "
				(upcase (format "%s" (nth (seq-position words keyword) words)))
				"  " (format "%s" (reverse onright))))
	  (setq onright '())
	  (setq onleft '())
	  (setq x 0)
	  (setq y 0)
	  (setcar (nthcdr (seq-position words keyword) words) "X")))
      (switch-to-buffer "*collocation*"))))

(defun linguistic-count-raw-word-list (raw-word-list)
  "Count the occurrences of each element in RAW-WORD-LIST and return an association list.  Function taken and modified from user xuchunyang on Stack Exchange in https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer."
  (let ((result nil))
    (cl-loop for elt in raw-word-list
	     do
	     (cl-incf (cdr (or (assoc elt result)
                             (cl-first (push (cons elt 0) result)))))
	     finally return (sort result
				  (lambda (a b) (> (cdr a) (cdr b)))))))

(defun linguistic-splitter (text)
  "Replace punctuation characters in TEXT and then split it."
  (split-string
   (replace-regexp-in-string linguistic-splitter-characters " " text)))

(defun linguistic-gram-stats (grams)
  "Apply ‘linguistic-count-raw-word-list’ to GRAMS."
  (linguistic-count-raw-word-list grams))

(defun linguistic-bigram ()
  "Read the buffer and return a list of all the bigrams and their occurrences in a new buffer."
  (interactive)
  (with-output-to-temp-buffer "*bigrams*"
    (let ((newlist '())
	  (words (linguistic-splitter (downcase (buffer-string)))))
      (cl-loop for item on words while (cl-rest item) do
	    (push (concat (cl-first item) " " (cl-second item)) newlist))
      (switch-to-buffer "*bigrams*")
      (print newlist)
      (print (linguistic-gram-stats newlist))
      (print (length newlist)))))

(defun linguistic-bigram-minibuffer ()
  "Read the buffer and return a list of all the bigrams in the minibuffer."
  (interactive)
  (let ((newlist '())
	(words (linguistic-splitter (downcase (buffer-string)))))
    (cl-loop for item on words while (cl-rest item) do
	  (push (concat (cl-first item) " " (cl-second item)) newlist))
    (print (reverse newlist))
    (print (length newlist))))

(defun linguistic-trigram-minibuffer ()
  "Read the buffer and return a list of all the trigrams in the minibuffer."
  (interactive)
  (let ((newlist '())
	(words (linguistic-splitter (downcase (buffer-string)))))
    (cl-loop for item on words while (cl-rest item) do
	  (push (concat (cl-first item) " " (cl-second item) " " (cl-third item)) newlist))
    (print (reverse newlist))
    (print (length newlist))))

(defun linguistic-trigram ()
  "Read the buffer and return a list of all the trigrams and the number of trigrams in a new buffer."
  (interactive)
  (with-output-to-temp-buffer "*trigrams*"
    (let ((newlist '())
	  (words (linguistic-splitter
		  (downcase (buffer-string)))))
      (cl-loop for item on words while (cl-rest item) do
	    (push (concat (cl-first item) " " (cl-second item) " " (cl-third item)) newlist))
      (switch-to-buffer "*trigrams*")
      (print (linguistic-gram-stats newlist)))))

(defun linguistic-trigram2 (text)
  (let ((newlist '())
	(words (linguistic-splitter
		(downcase text))))
    (cl-loop for item on words while (cl-rest item) do
	  (push (concat (cl-first item) " " (cl-second item) " " (cl-third item)) newlist))
    (linguistic-gram-stats newlist)))

(defun linguistic-bigram2 (text)
  "Return a list of all the bigrams in TEXT and their occurrences."
  (let ((newlist '())
	(words (linguistic-splitter
		(downcase text))))
    (cl-loop for item on words while (cl-rest item) do
	  (push (concat (cl-first item) " " (cl-second item)) newlist))
    (linguistic-gram-stats newlist)))

(defun linguistic-wordstopper (wordlist)
  "Delete items in WORDLIST matching the variable LINGUISTIC-STOPWORDS."
  (let ((final '()))
    (cl-loop for x in wordlist
	     do
	     (if (not (member x linguistic-stopwords))
		 (push x final))
	     finally return final)))

;; Function modified from user xuchunyang on Stack Exchange in https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer

(defun linguistic-word-freq ()
  "Return the most frequent words in a buffer or region in an org buffer ready to be plotted.  Finally, save a CSV copy of the org-table in the home dir.  Function modified from xuchunyang on Stack Exchange in https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer ."
  (interactive)
  (let* ((size (read-number "How long result list?:"))
	 (stopper (yes-or-no-p "Do you want to delete stopwords?"))
	 (words (if (use-region-p) (linguistic-splitter (downcase (buffer-substring (region-beginning) (region-end))))
		  (linguistic-splitter (downcase (buffer-string)))))
         (raw-word-list (if stopper (append (linguistic-wordstopper words)) (append words)))
         (word-list (linguistic-count-raw-word-list raw-word-list)))
    (with-current-buffer (get-buffer-create "*word-frequencies*")
      (erase-buffer)
      (insert "#+PLOT: WordFreqChart ind:1 set:\"style fill solid\" with:boxes set:\"boxwidth 0.7\" set:\"xrange [-0.5:10.5]\" set:\"yrange [0:]\" \n #+NAME: WordFreqChart \n")
      (insert "| word | occurrences |
               |-----------+------------|\n")
      (dolist (elt (cl-subseq word-list 0 size))
	(insert (format "| '%s' | %d |\n" (car elt) (cdr elt))))
      (insert "\n")
      (let ((graphs-dir (shell-command-to-string (format "find %s/working/ -name 'graphs.org'" package-user-dir))))
      (insert-file-contents (format "%s" (concat (file-name-directory graphs-dir) "graphs.org")))
	(org-mode)
	(indent-region (point-min) (point-max))
	(goto-char (point-min))
	(goto-char (search-forward-regexp "occurrences"))
	(org-cycle)
	(switch-to-buffer "*word-frequencies*")
	(goto-char (point-min))
	(goto-char (search-forward-regexp "occurrences"))
	(org-table-export "~/WordFreq.csv" "orgtbl-to-csv")
	(message "File  WordFreq.csv  created.")))))

;; Function modified from user xuchunyang on Stack Exchange in https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer

(defun linguistic-grams-freq ()
  "Return the most frequent bigrams or trigrams in a buffer or region in an org buffer ready to be plotted.  Finally, save a CSV copy of the org-table in the home dir.  Function modified from user xuchunyang on Stack Exchange in https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer ."
  (interactive)
  (let* ((size (read-number "How long result list:"))
	 (gram (read-number "Insert gram number (2 or 3):"))
	 (words (if (use-region-p) (downcase (buffer-substring (region-beginning) (region-end)))
		  (downcase (buffer-string))))
	 (word-list (if (= gram 3) (linguistic-trigram2 words) (linguistic-bigram2 words))))
    (with-current-buffer (get-buffer-create "*ngram-frequencies*")
      (erase-buffer)
      (insert "#+PLOT: NGramFreqChart ind:1 set:\"style fill solid\" with:boxes set:\"boxwidth 0.7\" set:\"xrange [-0.5:10.5]\" set:\"yrange [0:]\" \n #+NAME: WordFreqChart \n")
      (insert "| word | occurrences |
               |-----------+------------|\n")
      (dolist (elt (cl-subseq word-list 0 size))
	(insert (format "| '%s' | %d |\n" (car elt) (cdr elt))))
      (insert "\n")
      (let ((graphs-dir (shell-command-to-string (format "find %s/working/ -name 'graphs.org'" package-user-dir))))
	(insert-file-contents (format "%s" (concat (file-name-directory graphs-dir) "graphs.org")))
	(org-mode)
	(indent-region (point-min) (point-max))
	(goto-char (search-forward-regexp "occurrences"))
	(org-cycle)
	(switch-to-buffer "*ngram-frequencies*")
	(goto-char (point-min))
	(goto-char (search-forward-regexp "occurrences"))
	(org-table-export "~/GramFreq.csv" "orgtbl-to-csv")
	(message "File  GramFreq.csv  created.")))))

(define-minor-mode linguistic-mode
  "Minor mode that offers different tools for basic word frequency, collocation and bigram analysis.
  \\{linguistic-mode}"    
  :lighter "Linguistic Mode"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-w") 'linguistic-word-freq)
	    (define-key map (kbd "C-c C-g") 'linguistic-grams-freq)
	    (define-key map (kbd "C-c C-2") 'linguistic-bigram)
	    (define-key map (kbd "C-c C-3") 'linguistic-trigram)
	    map))

(provide 'linguistic)

;;; linguistic.el ends here
