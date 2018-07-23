(ns formula-parser.core-test
  (:require [clojure.test :refer :all]
            [formula-parser.core :refer :all]))

(deftest some-examples

  (testing "it does it"
    (is (vector? (parse "=IF(A1,ROUND(B1*100,C1+2),ROUND(B1,C1))")))

    (is (vector? (parse "ROUND(B1*100,C1+2)")))

    (is (vector? (parse "ROUND(B1*100)")))

    (is (vector? (parse "ROUND(B1,C1)")))

    (is (vector? (parse "B1*100")))

    (is (vector? (parse "={1,2;3,4}")))

    (is (vector? (parse "=SUM({1,2,3}*10)")))

    (is (vector? (parse "4")))

    (is (vector? (parse "TRUE")))

    (is (vector? (parse "\"hi\"")))

    (is (vector? (parse "=LOOKUP(A1,A3:A52,B3:B52)")))

    (is (vector? (parse "=SUMIF(A1:A5,\">4000\",B1:B5)")))

    (is (vector? (parse "=SUM(A2:A3)")))

    (is (vector? (parse "=A1")))

    (is (vector? (parse "=Cash_Flow!A1")))

    (is (vector? (parse "'Schedule E'!C7")))

    (is (vector? (parse "=IF(SUM(C121:C132)=(I21),SUM(C121:C132),\"Total off by \" & SUM(C121:C132)-I21)")))

    (is (vector? (parse "=+Tax!B3& \" \" &Tax!B5")))

    #_(parse "IF(1 & 2 & 3)")

    ))


