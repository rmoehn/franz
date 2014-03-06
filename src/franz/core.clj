(ns franz.core
  (:gen-class))

(def words ["abak" "abr" "abrakadabra" "bua" "cli" "kadab" "kadabr" "tul"])
(def many-words (clojure.string/split-lines
                  (slurp "/home/erle/Perl/franz/wordsEn.txt")))

;(defn mapa-b
;  "Map fn over the values from a (inclusive) to b (exclusive)."
;  ([func a b]
;    (mapa-b func a b 1))
;  "Map fn over the values from a (inclusive) to b (exclusive), going in steps
;  of size step."
;  ([func a b step]
;    (map fn (range a b step))))

(defn suffixes [min-len lst]
  "Return the proper suffixes (longest first) of lst with length >= min-len."
  (map #(subvec lst %) (range 1 (- (count lst) min-len -1))))

(defn prefixes [min-len lst]
  "Return the proper prefixes (longest first) of lst with length >= min-len."
  (map #(subvec lst 0 %) (reverse (range min-len (count lst)))))

(defn sublists [min-len lst]
  "Return a list of all sublists (longest first) of lst of length at least
  min-len."
  (mapcat #(partition % 1 lst) (reverse (range min-len (count lst)))))

(defn contained-words [min-len superword]
  "Returns a list of all the words w (length >= min-len) contained in
  superword, that is superword matches the regex (Perlish syntax) /.+ $w
  .+/x."
  (map (partial apply str)
       (sublists min-len
                 (subs superword 1 (dec (count superword))))))

(defn find-word [wordlist word]
  "Returns word if it is contained in wordlist, otherwise nil. wordlist must
  be ordered alphabetically."
  (if (>= (java.util.Collections/binarySearch wordlist word compare) 0)
    word))

(defn find-word-containing-word [min-len wordlist]
  "Returns a tuple of a word (length >= min-len) and a word that completely
  contains it from wordlist. wordlist must be ordered alphabetically."
  (let [superwords (shuffle (filter #(>= (count %) (+ 2 min-len)) wordlist))]
    (some (fn [superword]
        (some (fn [subword]
                (if-let [existing-subword (find-word wordlist subword)]
                  [superword existing-subword]))
              (contained-words min-len superword)))
      superwords)))

(defn find-overlap [min-len wordlist word]
  "If word is in wordlist and is constructed from the overlap of two words
  (length >= min-len) in wordlist, returns a tuple of these words, otherwise
  nil."
  (let [in-list?    (fn [wv] (find-word wordlist wv))
        wordvec     (vec word)
        begin-words (filter in-list? (map (partial apply str) (prefixes min-len wordvec)))
        end-words   (filter in-list? (map (partial apply str) (suffixes min-len wordvec)))]
    (if (and (not-empty begin-words) (not-empty end-words))
      [(first begin-words) (first end-words)])))


(defn find-two-remove-word [min-len wordlist]
  "Find a word w which can be used as an example for demonstrating the xform()
  function working on two remove operations. w will be as follows:

    c0 c1 c2 c3 c4 c5 c6 c7 ... cn

  where c0...cn (whole word) is in the wordlist and at least two derived words

    s1 = w0 \\ w(i1, j1)

    s2 = w0 \\ w(i2, j2)

    s0 = w0 \\ (s1 ∪ s2)


  are in the wordlist with i1 > 0, j1 > i, i2 < j1, j2 > i2, i2 < n. s1 and s2
  will be at least as long as min-len.

  This means, we want a word from the wordlist from which one, the other and
  both subwords can be removed and the outcome is in the wordlist everytime."
  (some (fn [[superword subword]]
          (if-let [[first-part second-part] (find-overlap min-len wordlist subword)]
            [superword subword first-part second-part]))
        (repeatedly #(find-word-containing-word (inc min-len) wordlist))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
