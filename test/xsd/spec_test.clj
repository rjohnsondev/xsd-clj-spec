(ns xsd.spec-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as spec]
            [clojure.spec.gen :as gen]
            [clojure.xml :as xml]
            [xsd.spec :as xsd])
  (:import [java.io StringReader StringBufferInputStream]
           [java.math BigDecimal]))


(deftest name-to-kw-test
  (is (= (xsd/name-to-keyword "test" "kw") :test/kw))
  (is (= (xsd/name-to-keyword "test" "kwCamelCase") :test/kw-camel-case))
  (is (nil? (xsd/name-to-keyword "test" nil))))

(deftest big-decimal-test
  (let [s (xsd/simple-type
            (xml/parse
              (StringBufferInputStream.
                "<xs:simpleType name=\"currencyRate\">
                <xs:restriction base=\"xs:decimal\">
                <xs:minExclusive value=\"3\" />
                <xs:maxExclusive value=\"9\" />
                </xs:restriction>
                </xs:simpleType>")))
        sc #(spec/explain-data s (BigDecimal. %))]
    (is (nil? (sc 4.2)))
    (is (not (nil? (sc 3))))
    (is (not (nil? (sc 9))))
    (is (not (nil? (sc 9.43))))))

(deftest big-decimal-digit-test
  (let [s (xsd/simple-type
            (xml/parse
              (StringBufferInputStream.
                "<xs:simpleType name=\"currencyRate\">
                <xs:restriction base=\"xs:decimal\">
                <xs:totalDigits value=\"5\" />
                <xs:fractionDigits value=\"2\" />
                </xs:restriction>
                </xs:simpleType>")))
        sc #(spec/explain-data s (BigDecimal. %))]
    (is (nil? (sc "4.21")))
    (is (not (nil? (sc "4.5555"))))
    (is (not (nil? (sc "455555"))))))

(deftest big-decimal-generator-test
  (let [s (xsd/simple-type
            (xml/parse
              (StringBufferInputStream.
                "<xs:simpleType name=\"currencyRate\">
                <xs:restriction base=\"xs:decimal\">
                <xs:minExclusive value=\"10\" />
                <xs:maxExclusive value=\"1000\" />
                <xs:totalDigits value=\"5\" />
                <xs:fractionDigits value=\"1\" />
                </xs:restriction>
                </xs:simpleType>")))
        g (spec/gen s)]
    (is (>= 10 (count (gen/sample g))))))


(deftest float-test
  (let [s (xsd/simple-type
            (xml/parse
              (StringBufferInputStream.
                "<xs:simpleType name=\"currencyRate\">
                <xs:restriction base=\"xs:float\">
                <xs:minExclusive value=\"1.234567\" />
                <xs:maxExclusive value=\"100000000000\" />
                </xs:restriction>
                </xs:simpleType>")))
        sc #(spec/explain-data s %)]
    (is (nil? (sc 4.21)))
    (is (not (nil? (sc 1.0))))))

(deftest enum-test
  (let [s (xsd/simple-type
            (xml/parse
              (StringBufferInputStream.
                "<xs:simpleType name=\"lineAmountType\">
                <xs:restriction base=\"xs:string\">
                <xs:enumeration value=\"NoTax\"></xs:enumeration>
                <xs:enumeration value=\"Inclusive\"></xs:enumeration>
                <xs:enumeration value=\"Exclusive\"></xs:enumeration>
                </xs:restriction>
                </xs:simpleType>")))
        sc #(spec/explain-data s %)
        g (spec/gen s)]
    (is (nil? (sc "NoTax")))
    (is (not (nil? (sc "NoTax1"))))
    (is (>= 10 (count (gen/sample g))))))

(deftest enum-anon-test
  (let [s (xsd/simple-type
            (xml/parse
              (StringBufferInputStream.
                "<xs:simpleType>
                <xs:restriction base=\"xs:string\">
                <xs:enumeration value=\"NoTax\"></xs:enumeration>
                <xs:enumeration value=\"Inclusive\"></xs:enumeration>
                <xs:enumeration value=\"Exclusive\"></xs:enumeration>
                </xs:restriction>
                </xs:simpleType>")))
        sc #(spec/explain-data s %)
        g (spec/gen s)]
    (is (nil? (sc "NoTax")))
    (is (not (nil? (sc "NoTax1"))))
    (is (>= 10 (count (gen/sample g))))))

(deftest simple-list-test
  (let [s (xsd/list-of
            (xml/parse
              (StringBufferInputStream.
                "<xs:simpleType name=\"lineAmountType\">
                <xs:list itemType=\"xs:float\"></xs:list>
                </xs:simpleType>")))
        sc #(spec/explain-data s %)
        g (spec/gen s)]
    (is (nil? (sc [0.3 4.4 4.0])))
    (is (not (nil? (sc [:asdf 0.3 4.4 4.0]))))
    (is (>= 10 (count (gen/sample g))))))

(deftest derived-list-test
  (let [s (xsd/list-of
            (xml/parse
              (StringBufferInputStream.
                "<xs:simpleType name=\"lineAmountType\">
                <xs:list>
                <xs:simpleType>
                <xs:restriction base=\"xs:string\">
                <xs:enumeration value=\"NoTax\"></xs:enumeration>
                <xs:enumeration value=\"Inclusive\"></xs:enumeration>
                <xs:enumeration value=\"Exclusive\"></xs:enumeration>
                </xs:restriction>
                </xs:simpleType>
                </xs:list>
                </xs:simpleType>")))
        sc #(spec/explain-data s %)
        g (spec/gen s)]
    (is (nil? (sc ["NoTax" "Inclusive"])))
    (is (not (nil? (sc ["Inclusive" "NoTax1"]))))
    (is (>= 10 (count (gen/sample g))))))

;(xsd/list-of
;  "test-ns"
;  (xml/parse
;    (StringBufferInputStream.
;      "<xs:simpleType name=\"lineAmountType\">
;      <xs:list>
;      <xs:simpleType>
;      <xs:restriction base=\"xs:string\">
;      <xs:enumeration value=\"NoTax\"></xs:enumeration>
;      <xs:enumeration value=\"Inclusive\"></xs:enumeration>
;      <xs:enumeration value=\"Exclusive\"></xs:enumeration>
;      </xs:restriction>
;      </xs:simpleType>
;      </xs:list>
;      </xs:simpleType>")))
