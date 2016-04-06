(ns looping-is-recursion)

(defn power [base exp]
  (loop [e exp
         acc base]
    (cond
      (zero? e) 1
      (= e 1) acc
      :else (recur (dec e)
                   (* acc base)))))

(defn last-element [a-seq]
  (loop [head (first a-seq)
         tail (seq (rest a-seq))]
    (cond
      (nil? head) nil
      (nil? tail) head
      :else (recur (first tail)
                   (seq (rest tail))))))

(defn seq= [seq1 seq2]
  (loop [a-seq (seq seq1)
         b-seq (seq seq2)]
    (cond
      (nil? a-seq) (nil? b-seq)
      (nil? b-seq) (nil? a-seq)
      (not= (first a-seq) (first b-seq)) false
      :else (recur (seq (rest a-seq))
                   (seq (rest b-seq))))))

(defn find-first-index [pred a-seq]
  (loop [coll (seq a-seq)
         acc 0]
    (cond
      (nil? coll) nil
      (pred (first coll)) acc
      :else (recur (seq (rest coll))
                   (inc acc)))))

(defn avg [a-seq]
  (loop [num 0
         total 0
         coll (seq a-seq)]
    (if (nil? coll)
      (/ total num)
      (recur (inc num)
             (+ total (first coll))
             (seq (rest coll))))))

(defn toggle [a-set elem]
  (if (a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [coll (seq a-seq)
         acc #{}]
    (if (nil? coll)
      acc
      (recur (seq (rest coll))
             (toggle acc (first coll))))))

(defn fast-fibo [n]
  (loop [num n
         a 0
         b 1]
    (if (zero? num)
      a
      (recur (dec num)
             (+ a b)
             a))))

(defn cut-at-repetition [a-seq]
  (loop [coll (seq a-seq)
         test #{}
         acc []]
    (if (or (nil? coll)
            (test (first coll)))
      (seq acc)
      (recur (seq (rest coll))
             (conj test (first coll))
             (conj acc (first coll))))))

