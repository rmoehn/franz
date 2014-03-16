(ns franz.core
  (:gen-class))

(def words ["abak" "abr" "abrakadabra" "bua" "cli" "kadab" "kadabr" "tul"])
(def many-words (clojure.string/split-lines
                  (slurp "/home/erle/Perl/franz/wordsEn.txt")))

(defn find-word [wordlist word]
  "Returns word if it is contained in wordlist, otherwise nil. wordlist must
  be ordered alphabetically."
  (if (>= (java.util.Collections/binarySearch wordlist word compare) 0)
    word))

(defn fixed-lower-pairs
  "Returns a sequence of all ordered pairs with lower as left element and the
  numbers from (lower + min-diff) to upper (inclusive) as right elements."
  ([lower upper]
   (fixed-lower-pairs lower upper 1))
  ([lower upper min-diff]
   {:pre [(<= lower upper)
          (<= 0 min-diff)]
    :post [(every? (fn [ [l u] ] (<= min-diff (- u l))) %)]}
   (map (fn [u] [lower u]) (range (+ lower min-diff) (inc upper)))))

(defn ordered-pairs
  "Returns a sequence of all ordered pairs of the numbers from lower
  (inclusive) to upper (inclusive), where the difference between the two
  numbers is at least min-diff."
  ([lower upper]
   (ordered-pairs lower upper 1))
  ([lower upper min-diff]
   (mapcat #(fixed-lower-pairs % upper min-diff)
           (range lower (inc upper)))))

(defn overlapping-pairs
  "Returns a sequence of pairs of overlapping intervals, i. e. pairs [p₁ p₂]
  of ordered pairs p₁ = [l₁ u₁] and p₂ = [l₂ u₂], where l₂ ≦ u₁. See the pre-
  and postconditions for a detailed specification."
  [lower upper & {:keys [min-ludiff min-overlap min-ldiff min-udiff]
                  :or   {min-ludiff  1
                         min-overlap 1
                         min-ldiff   1
                         min-udiff   1}}]
  {:pre  [(<= lower upper)
          (<= 0 min-overlap)
          (<= 0 min-ldiff)
          (<= 0 min-udiff)]
   :post [(every? (fn [ [[l1 u1] [l2 u2]] ]
                    (and (<= lower l1)
                      (<= u2 upper)
                      (>= (- u1 l2) (dec min-overlap))
                      (>= (- l2 l1) min-ldiff)
                      (>= (- u2 u1) min-udiff))) %)]}
  (mapcat (fn [ [l1 u1] ]
            (map (fn [p2] [ [l1 u1] p2 ])
                 (mapcat (fn [l2]
                           (fixed-lower-pairs
                             l2
                             upper
                             (max min-ludiff
                                  (+ (- u1 l2) min-udiff))))
                         (range (+ l1 min-ldiff) (+ (- u1 min-overlap) 2)))))
          (ordered-pairs lower (- upper min-udiff) min-ludiff)))

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
