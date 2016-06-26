(ns xsd.spec
  (:require [clojure.spec :as spec]
            [clojure.walk :as walk]
            [clojure.xml :as xml]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.spec.gen :as g]
            [clojure.test.check.generators :as gen])
  (:import [java.io StringReader StringBufferInputStream]
           [java.math BigDecimal]))

;; TODO: this needs to conform to the XML api
;;  Should be able to utilise multi-spec functionality, basing things on tag

;(xml/parse
;  (doto (io/input-stream
;          (io/resource "XeroAPI-Schemas/v2.00/BaseTypes.xsd"))
;    (.read) ; skip over the BOM
;    (.read)
;    (.read)))

(def type-ns "xsd")

(defn name-to-keyword
  ([n]
   (name-to-keyword nil n))
  ([ns n]
   (if (nil? n)
     nil
     (let [[_ pre wrds] (re-find #"([a-z]+)([a-zA-Z]+|$)" n)]
       (keyword
         ns
         (reduce
           (fn [out wrd]
             (str
               out
               "-"
               (s/lower-case wrd)))
           pre
           (re-seq #"[A-Z][a-z]+" wrds)))))))

;(re-find #"([a-z]+)([a-zA-Z]+|$)" "simpleType")
;(s/replace "1234.423" #"[^0-9]" "")

(def numeric-restriction-map
  {:xs:minExclusive (fn [x]
                      (spec/def-impl 
                        (keyword "xsd/constraint" (str "min-exclusive-" x))
                        (str "#(> % " x ")")
                        #(> % x)))
   :xs:minInclusive (fn [x]
                      (spec/def-impl 
                        (keyword "xsd/constraint" (str "min-inclusive-" x))
                        (str "#(>= % " x ")")
                        #(>= % x)))
   :xs:maxExclusive (fn [x]
                      (spec/def-impl 
                        (keyword "xsd/constraint" (str "max-exclusive-" x))
                        (str "#(< % " x ")")
                        #(< % x)))
   :xs:maxInclusive (fn [x]
                      (spec/def-impl 
                        (keyword "xsd/constraint" (str "max-inclusive-" x))
                        (str "#(<= % " x ")")
                        #(<= % x)))})

(def restriction-map
  {:xsd/decimal (merge
                  numeric-restriction-map
                  {:xs:totalDigits (fn [x]
                                     (spec/def-impl 
                                       (keyword "xsd/constraint" (str "total-digits-" x))
                                       (str "#(>= " x
                                            "  (.length (s/replace (.toPlainString %) #\"[^0-9]\" \"\")))")
                                       #(>= x (.length (s/replace (.toPlainString %) #"[^0-9]" "")))))
                   :xs:fractionDigits (fn [x]
                                        (spec/def-impl 
                                          (keyword "xsd/constraint" (str "fraction-digits-" x))
                                          (str "#(>= " x
                                               " (max 0 (.scale (.stripTrailingZeros x))))")
                                          #(>= x (max 0 (.scale (.stripTrailingZeros %))))))})
   :xsd/float numeric-restriction-map
   :xsd/string {

                }
   })

(defn type-keyword [ts]
  (if (nil? ts)
    nil
    (keyword
      type-ns
      (s/replace ts #"^xs:" ""))))

(defn numeric-range [crc rs]
  (reduce
    (fn [out r]
      (let [v (crc (get-in r [:attrs :value]))]
        (case (get r :tag)
          :xs:minInclusive (update out :min #(if (nil? %) v (max v %))) ; go for the more restrictive
          :xs:minExclusive (update out :min #(if (nil? %) (inc v) (max (inc v) %)))
          :xs:maxInclusive (update out :max #(if (nil? %) v (min v %)))
          :xs:maxExclusive (update out :max #(if (nil? %) (dec v) (min (dec v) %)))
          out)))
    {}
    rs))

;; get the base types into the ns
(spec/def
  :xsd/decimal
  (spec/with-gen
    #(instance? BigDecimal %)
    (fn [] (g/fmap
             #(if (instance? BigDecimal %)
                %
                (BigDecimal. %))
             (gen/double*
               {:infinite? false
                :NaN? false})))))

(spec/def
  :xsd/float
  (spec/with-gen
    #(instance? Double %)
    (fn [] gen/double)))

(spec/def
  :xsd/string
  (spec/with-gen
    #(instance? String %)
    (fn [] gen/string)))

(defn constrained-numeric-gen [bt mm]
  (case bt
    :xsd/decimal (fn [] (g/fmap
                               #(if (instance? BigDecimal %)
                                  %
                                  (BigDecimal. %))
                               (gen/double*
                                 {:infinite? false
                                  :NaN? false
                                  :min (get mm :min)
                                  :max (get mm :max)})))
    :xsd/float (fn [] (gen/double*
                             {:min (get mm :min)
                              :max (get mm :max)}))))

(defn coercer [bt]
  (case bt
    :xsd/decimal #(BigDecimal. %)
    :xsd/float #(Double/parseDouble %)
    :xsd/string #(.toString %)))

(defn enums [n rs]
  (into
    #{}
    (map
      #(get-in % [:attrs :value])
      (filter
        #(= (get % :tag) :xs:enumeration)
        rs))))

(defn constraints [bt crc rs]
  (map
    (fn [c]
      ((get-in
         restriction-map
         [bt (keyword (get c :tag))])
       (crc (get-in c [:attrs :value]))))
    (remove
      #(= (get % :tag) :xs:enumeration)
      rs)))

(defn spec-from-constraints [btn bt cstrs]
  (spec/and-spec-impl
    (into
      [btn]
      (mapv name cstrs))
    (into
      [bt]
      cstrs)
    nil))

(defn simple-type [xml]
  (let [n (name-to-keyword type-ns (get-in xml [:attrs :name]))
        r (first
            (filter
              #(= (get % :tag) :xs:restriction)
              (get xml :content)))
        bt (type-keyword (get-in r [:attrs :base]))
        crc (coercer bt)
        rs (get r :content)
        mm (numeric-range crc rs)
        t (if (empty? mm)
            bt
            (spec/with-gen bt
              (constrained-numeric-gen bt mm)))
        enum-vals (enums 
                    (get-in xml [:attrs :name])
                    (get r :content))
        cstrs (constraints bt crc rs)]
    (if (nil? n)
      (if (empty? enum-vals)
        (spec/spec (spec-from-constraints (name bt) t cstrs))
        (spec/spec enum-vals))
      (if (empty? enum-vals)
        (spec/def
          n
          (spec-from-constraints (name bt) t cstrs))
        (spec/def n enum-vals)))))

;(defn validate-list [bt txt]
;  (let [crc (coercer bt)]
;    (reduce
;      (fn [out p]
;        (and out
;             (spec/valid?
;               bt
;               (try
;                 (crc p)
;                 (catch IllegalArgumentException err
;                   false)))))
;      true
;      (s/split txt #"\s"))))
;
;(spec/explain-data
;  (spec/spec
;    #(validate-list :xsd/decimal %))
;  "2s3 23.45 1234")
;
;(spec/explain-data
;  (spec/spec
;    #(validate-list :xsd/float %))
;  "2s3 23.45 1234")

(defn list-of [xml]
  (let [n (name-to-keyword type-ns (get-in xml [:attrs :name]))
        lit (type-keyword (get-in (first (get xml :content)) [:attrs :itemType]))
        c (first (get (first (get xml :content)) :content))
        bt (or lit (simple-type c))]
    (spec/* bt)))

; (def tmp
;   (xml/parse
;     (StringBufferInputStream.
;       "<xs:simpleType name=\"currencyRate\">
;       <xs:restriction base=\"xs:decimal\">
;       <xs:minExclusive value=\"3\" />
;       <xs:maxExclusive value=\"9\" />
;       </xs:restriction>
;       </xs:simpleType>")))
; (simple-type "invoice" tmp)
; 
; (spec/explain :invoice/currency-rate (BigDecimal. 6))
; 
; (g/sample (spec/gen :invoice/currency-rate))
; 
; (into [] (take 20 (g/sample (spec/gen :invoice/currency-rate))))
; 
; 
; (g/sample
;   (spec/gen
;     (spec/and
;       (spec/with-gen
;         (spec/def ::tmp4 #(instance? BigDecimal %))
;         (fn [] (g/fmap #(BigDecimal. %) gen/double)))
;       (spec/spec #(> % 3)))))))
; 
; (spec/gen :invoice/currency-rate)))
; 
; (spec/def ::tmp3 int?)
; (spec/gen ::tmp3)
; (spec/valid? ::tmp3 2)
; 
; (name
;   (spec/def :xsd/decimal #(instance? BigDecimal %)))
; 
; (name
; (spec/with-gen
;   (spec/def :xsd/decimal #(instance? BigDecimal %))
;   (fn [] (g/fmap #(BigDecimal. %) gen/double))))
; 
; (g/sample 
; (spec/gen
; ((get gen-restriction-map :xs:minExclusive)
;  (BigDecimal. 1))))
; 
; 
; (spec/def ::min-test #(> % 1))
; 
; (spec/gen int?)
; 
; (gen-min-exclusive 5)
; (spec/explain-data :xsd/min-exclusive-5 0)
; 
; (spec/def-impl ::tmp "#(> % 1)" #(> % 1))
; (spec/def ::tmp2 (spec/and-spec-impl
;                    ['::tmp 'even?]
;                    [::tmp even?]
;                    nil))
; 
; (spec/explain-data ::tmp 1)
; (spec/explain-data ::tmp2 0)
; (spec/valid? ::tmp2 2)
; 
; (spec/explain-data :invoice/currency-rate (BigDecimal. "-1"))
