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

(defn encode-char [key-char message-char]
  (let [key-index (index-of alphabet key-char)
        message-index (index-of alphabet message-char)]
    (nth (nth shifted-alphabets key-index) message-index)))

(defn decode-char [key-char message-char]
  (let [key-index (index-of alphabet key-char)]
    (nth alphabet (index-of (nth shifted-alphabets key-index) message-char))))

(defn encode [keyword message]
  (apply str (map encode-char (cycle keyword) message)))

(defn decode [keyword message]
  (apply str (map decode-char (cycle keyword) message)))

(defn decipher [cipher message]
  "decypherme")
