(ns formula-parser.core-test
  (:require [clojure.test :refer :all]
            [formula-parser.core :refer :all]
            [clojure.java.io :as io]))

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

    (is (vector? (parse "=A21+1")))

    (is (vector? (parse "=A21 + 1")))

    (is (vector? (parse "SUM(IF((DelPoint= \"PV\"), 1, 2))")))

    (is (vector? (parse "=D106*0.01")))

    (is (vector? (parse "[1]!'NGH1,PRIM ACT 1'")))

    (is (vector? (parse "(YEAR(I14)-YEAR(C4))*12+MONTH(I14)-MONTH(C4)+IF(MONTH(I14)-MONTH(C4)>=0, IF((DAY(I14)-DAY(C4))>25, 1,( IF(DAY(I14)-DAY(C4)<-5, -1,0))), IF((DAY(I14)-DAY(C4))>25, 1, IF((DAY(I14)-DAY(C4))<-25, -1,0)))")))

    (is (vector? (parse "'[6]HYDRO-EBITDA'!I$1")))


    ))

(defmacro build-that-enron-shit []
  (let [enron (io/reader "enronformulas.txt")
        line-groups (->> enron
                         line-seq
                         (map (fn [line]
                                 (-> line
                                     (subs 1 (- (count line) 1))
                                     (clojure.string/replace #"\"\"" "\""))))
                         (partition-all 300)
                         #_(drop 160)
                         #_(take 10)
                         (take 160)
                         )]
    `(do

       ~@(for [[line-group# i#] (map (fn [lg i] [lg i])
                                     line-groups
                                     (range))]
           `(deftest ~(symbol (str "enron-formulas-" i#))
              (testing ~(str "it does it " i#)
                ~@(for [line# line-group#]
                    `(is (vector? (parse ~line#))))))))))

(build-that-enron-shit)

