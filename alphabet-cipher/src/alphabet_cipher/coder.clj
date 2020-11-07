(ns alphabet-cipher.coder)

(defn zip [& collections]
  (apply map vector collections))

(defn char-range [incl-start incl-end]
  (let [incl-start (int incl-start)
        excl-end (inc (int incl-end))]
    (map char (range incl-start excl-end))))

(defn shift
  ([collection] (concat (rest collection) [(first collection)]))
  ([n collection] (concat (drop n collection) (take n collection))))

(def alphabet (char-range \a \z))

(def shifted-alphabets
  (map-indexed shift (repeat (count alphabet) alphabet)))

(defn index-of [coll item]
  (count (take-while #(not= item %) coll)))

(defn find-keyword-repetition [coll]
  (let [coll-count (count coll)
        coll-slices (map-indexed (fn [index _item] (take index coll)) coll)
        result (first (filter #(= coll (take coll-count (cycle %))) coll-slices))]
    (or result coll))
  )

(defn encode-char [key-char message-char]
  (let [key-index (index-of alphabet key-char)
        message-index (index-of alphabet message-char)]
    (nth (nth shifted-alphabets key-index) message-index)))

(defn decode-char [key-char message-char]
  (let [key-index (index-of alphabet key-char)]
    (nth alphabet (index-of (nth shifted-alphabets key-index) message-char))))

(defn decipher-char [cipher-char message-char]
  (let [message-index (index-of alphabet message-char)]
    (nth alphabet (index-of (nth shifted-alphabets message-index) cipher-char))))

(defn encode [keyword message]
  (apply str (map encode-char (cycle keyword) message)))

(defn decode [keyword message]
  (apply str (map decode-char (cycle keyword) message)))

(defn decipher [cipher message]
  (apply str (find-keyword-repetition (map decipher-char cipher message))))
