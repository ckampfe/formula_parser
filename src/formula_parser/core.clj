(ns formula-parser.core
  (:require [instaparse.core :as ip]
            [instaparse.combinators :refer :all]
            [clojure.java.io :as io])
  (:gen-class))

(def function-list
  (->> "function_list.txt"
       io/resource
       slurp
       clojure.string/split-lines
       (map clojure.string/trim)))

#_(defn char-range [start end]
    (map (comp str char) (range (int start) (inc (int end)))))

(def formula-bnf
  {:Start (alt (nt :Constant)
               (cat (hide (opt (string "="))) (nt :Formula))
               (nt :ArrayFormula))

   :ArrayFormula (cat (hide (string "{="))
                      (nt :Formula)
                      (hide (string "}")))

   :Formula (alt (nt :Constant)
                 (nt :Reference)
                 (nt :FunctionCall)
                 (cat (hide (string "("))
                      (nt :Formula)
                      (hide (string ")")))
                 (nt :ConstantArray)
                 (nt :RESERVED-NAME))

   :Constant (alt (nt :NUMBER)
                  (nt :STRING)
                  (nt :BOOL)
                  (nt :ERROR))

   :FunctionCall (ord (cat (nt :Function) (nt :Arguments) (string ")"))
                      (cat (nt :UnOpPrefix) (nt :Formula))
                      (cat (nt :Formula) (string "%"))
                      (cat (nt :Formula) (nt :BinOp) (nt :Formula)))

   :UnOpPrefix (alt (string "+") (string "-"))

   :BinOp (->> ["+" "-" "*" "/" "^" "<" ">" "=" "<=" ">=" "<>"]
               (map string)
               (apply alt))

   :Function (ord (nt :FUNCTION)
                  (nt :UDF))

   :Arguments (alt (nt :Argument)
                   (cat (nt :Argument)
                        (string ",")
                        (nt :Arguments)))

   :Argument (alt (nt :Formula) #_Epsilon)

   :Reference (alt (nt :ReferenceItem)
                   (cat (nt :Reference)
                        (string ":")
                        (nt :Reference))
                   (cat (nt :Reference)
                        (string " ")
                        (nt :Reference))
                   (cat (hide (string "("))
                        (nt :Union)
                        (hide (string ")")))
                   (cat (hide (string "("))
                        (nt :Reference)
                        (hide (string ")")))
                   (cat (nt :Prefix) (nt :ReferenceItem))
                   (cat (nt :Prefix) (nt :UDF) (nt :Arguments) (hide (string ")")))
                   (nt :DynamicDataExchange))

   :ReferenceItem (alt (nt :CELL)
                       (nt :NamedRange)
                       (cat (nt :REFERENCE-FUNCTION) (nt :Arguments) (hide (string ")")))
                       (nt :VERTICAL-RANGE)
                       (nt :HORIZONTAL-RANGE)
                       (nt :ERROR-REF))

   :Prefix (alt (nt :SHEET)
                (cat (nt :FILE) (nt :SHEET))
                (cat (nt :FILE) (string "!"))
                (nt :QUOTED-FILE-SHEET)
                (nt :MULTIPLE-SHEETS)
                (cat (nt :FILE) (nt :MULTIPLE-SHEETS)))

   :NamedRange (alt (nt :NAMED-RANGE)
                    (nt :NAMED-RANGE-PREFIXED))

   :Union (alt (nt :Reference)
               (cat (nt :Reference) (string ",") (nt :Union)))

   :DynamicDataExchange (cat (nt :FILE) (string "!") (nt :DDECALL))

   :ConstantArray (cat (hide (string "{"))
                       (nt :ArrayColumns)
                       (string "}"))

   :ArrayColumns (alt (nt :ArrayRows)
                      (cat (nt :ArrayRows) (hide (string ";")) (nt :ArrayColumns)))

   :ArrayRows (alt (nt :ArrayConstant)
                   (cat (nt :ArrayConstant) (hide (string ",")) (nt :ArrayRows)))

   :ArrayConstant (alt (nt :Constant)
                       (cat (nt :UnOpPrefix) (nt :NUMBER))
                       (nt :ERROR-REF))

   ;; TOKENS

   ;; TRUE | FALSE
   :BOOL (alt (string "TRUE")
              (string "FALSE"))

   ;; $? [A-Z]+ $? [0-9]+
   :CELL (regexp "$? [A-Za-z]+ $? [0-9]+")

   ;; :CELL (cat (opt (string "$"))
   ;;            (plus (apply alt (map string (char-range \a \z))))
   ;;            (opt (string "$"))
   ;;            (plus (apply alt (map string (map str (range 0 10)))))
   ;;            )

   ;; ’ ([A-Z0-9_ !@#$%^&*()+={}:;|<>,./?\\] | ”)+ ’
   :DDECALL (string "")

   ;; #NULL! | #DIV/0! | #VALUE! | #NAME? | #NUM! | #N/A
   :ERROR (alt (string "#NULL!")
               (string "#DIV/0!")
               (string "#VALUE!")
               (string "#NAME?")
               (string "#NUM!")
               (string "#N/A"))

   ;; #REF!
   :ERROR-REF (string "#REF!")

   ;; \[ [0-9]+ \]
   :FILE (cat (string "[")
              (regexp "[0-9]+")
              (string "]"))

   ;; (Any entry from the function list1) \(
   :FUNCTION (cat (apply alt (map string function-list))
                  (hide (string "(")))

   ;; $? [0-9]+ : $? [0-9]+
   :HORIZONTAL-RANGE (regexp "$? [0-9]+ : $? [0-9]+")

   ;; [A-Z0-9]+ : ([A-Z0-9_.]+ | ’ ([A-Z0-9_ !^&*()+={}:;|<>,./?\\] | ”)+ ’) !
   :MULTIPLE-SHEETS
   (let [initial (regexp "[A-Za-z0-9]+")
         colon (string ":")
         single-quote (string "'")
         double-quote (string "\"")
         bang (string "!")
         big (alt (regexp "[A-Za-z0-9_\\.]+")
                  (cat single-quote
                       (plus (alt (regexp "[A-Z0-9_ \\!\\^\\&\\*\\(\\)\\+\\=\\{\\}\\:\\;\\|\\<\\>\\,\\.\\/\\?\\\\]")
                                  double-quote))
                       single-quote))]
     (cat initial
          colon
          big
          bang))

   ;; [A-Z_][A-Z0-9_.]*
   :NAMED-RANGE (regexp "[A-Z_][A-Z0-9_.]*")

   ;; (TRUE | FALSE | [A-Z]+[0-9]+) [A-Z0-9_.]+
   :NAMED-RANGE-PREFIXED (string "");; [0-9]+ ,? [0-9]* (e [0-9]+)?

   ;; [0-9]+ ,? [0-9]* (e [0-9]+)?
   :NUMBER (cat (regexp "[0-9]+")
                (regexp ",?")
                (regexp "[0-9]*")
                (opt (cat (string "e") (regexp "[0-9]+"))))

   ;; ’\[ [0-9]+ \] ([0-9A-Z_ !@#$%^&*()+={}:;|<>,./?\\] | ”)+ ’!
   :QUOTED-FILE-SHEET (let [big-regexp
                            (regexp #"([0-9A-Za-z_ \\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\+\\=\\|\\:\\;\\<\\>\\,\\.\\?\\\\\\\"])+")]

                        big-regexp
                        (cat (hide (string "'["))
                             (regexp "[0-9]+")
                             (hide (string "]"))
                             big-regexp
                             (hide (string "'!"))))

   ;; (INDEX | OFFSET | INDIRECT)\(
   :REFERENCE-FUNCTION (cat (alt (string "INDEX")
                                 (string "OFFSET")
                                 (string "INDIRECT"))
                            (string "("))

   ;; _xlnm\. [A-Z_]+
   :RESERVED-NAME (cat (string "_xlnm.")
                       (regexp "[A-Za-z_]+"))

   ;; ([0-9A-Z_.]+ | ’ ([0-9A-Z_ !@#$%^&*()+=|:;<>,./?\\] | ”)+ ’) !
   :SHEET (let [big-regexp (alt (regexp "0-9A-Za-z_ ")
                                (->> (clojure.string/split "!@#$%^&*()+=|:;<>,./?\\" #"")
                                     (map string)
                                     (apply alt)))]

            (cat (alt (regexp "[0-9A-Za-z_.]+")
                      (cat (string "'")
                           (plus (alt big-regexp
                                      (string "\"")))
                           (string "'")))
                 (string "!")))

   ;; " ([^ "]|"")* "
   :STRING (cat (hide (string "\""))
                (regexp "([^ \"])*")
                (hide (string "\"")))

   ;; (_xll\.)? [A-Z0-9]+ (
   :UDF (cat (opt (string "_xll."))
             (regexp "[A-Za-z0-9]+")
             (hide (string "(")))

   ;; $? [A-Z]+ : $? [A-Z]+
   :VERTICAL-RANGE (regexp "$? [A-Z]+ : $? [A-Z]+")})

(def formula-parser (ip/parser formula-bnf :start :Start))

(ip/parse formula-parser  "=IF(A1,ROUND(B1*100,C1+2),ROUND(B1,C1))")

(ip/parse formula-parser "={1,2;3,4}")

(ip/parse formula-parser "=SUM({1,2,3}*10)")

(ip/parse formula-parser "4")

(ip/parse formula-parser "TRUE")

(ip/parse formula-parser "\"hi\"")

(clojure.pprint/pprint (ip/parse formula-parser "=LOOKUP(A1,A3:A52,B3:B52)"))

(ip/parse formula-parser "=SUMIF(A1:A5,\">4000\",B1:B5)")

(clojure.pprint/pprint (ip/parse formula-parser "=SUM(A2:A3)"))

(ip/parse formula-parser "=A1")

(ip/parse formula-parser "=Cash_Flow!A1")

;; quoted file sheet
(clojure.pprint/pprint (ip/parse formula-parser "=('[2]Detail I&E'!D62)/1000"))



;; udf
(ip/parse formula-parser "=[1]!wbname()")

(defn parse [s]
  (ip/parse formula-parser s))

(parse "SUM(Sheet2:Sheet5!A11)")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
