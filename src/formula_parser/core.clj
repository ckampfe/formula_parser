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

(def formula-bnf
  {:Start (alt (cat (star (string " ")) (nt :Constant) (star (string " ")))
               (cat (hide (opt (string "=")))
                    (hide (star (string " ")))
                    (nt :Formula))
               (nt :ArrayFormula))

   :ArrayFormula (cat (hide (string "{="))
                      (nt :Formula)
                      (hide (string "}")))

   ;; NOTE: THIS IS NOT PART OF THE GRAMMAR PAPER
   ;; NOTE: it is also disgusting
   :FormulaWithJoin (cat (neg (nt :FormulaWithJoin))
                         (nt :Formula)
                         (plus (cat (hide (cat (star (string " "))
                                               (string "&")
                                               (star (string " "))))
                                    (neg (nt :FormulaWithJoin))
                                    (nt :Formula))))

   :Formula (alt (nt :Constant)
                 (nt :Reference)
                 (nt :FunctionCall)
                 (cat (hide (string "("))
                      (hide (star (string " ")))
                      (nt :Formula)
                      (hide (star (string " ")))
                      (hide (string ")")))
                 (nt :ConstantArray)
                 (nt :FormulaWithJoin)  ;; NOTE: THIS IS NOT PART OF THE GRAMMAR PAPER
                 (nt :RESERVED-NAME))

   :Constant (alt (nt :NUMBER)
                  (nt :STRING)
                  (nt :BOOL)
                  (nt :ERROR))

   :FunctionCall (alt (cat (nt :Function) (nt :Arguments) (hide (string ")")))
                      (cat (nt :UnOpPrefix) (nt :Formula))
                      (cat (nt :Formula) (string "%"))
                      (cat (nt :Formula) (nt :BinOp) (nt :Formula)))

   :UnOpPrefix (alt (string "+") (string "-"))

   :BinOp (->> ["+" "-" "*" "/" "^" "<" ">" "=" "<=" ">=" "<>"]
               (map (fn [s] (cat (hide (star (string " ")))
                                 (string s)
                                 (hide (star (string " "))))))
               (apply alt))

   :Function (ord (nt :FUNCTION)
                  (nt :UDF))

   :Arguments (alt (nt :Argument)
                   (cat (nt :Argument)
                        (plus (cat (hide (string ","))
                                   (nt :Argument)))))

   :Argument (alt (cat (star (string " "))
                       (nt :Formula)
                       (star (string " "))
                       )
                  Epsilon)

   :Reference (alt (nt :ReferenceItem)
                   (cat (nt :Reference)
                        (hide (string ":"))
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

   :ReferenceItem (ord (nt :CELL)
                       (nt :NamedRange)
                       (cat (nt :REFERENCE-FUNCTION)
                            (nt :Arguments)
                            (hide (string ")")))
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
   :CELL (cat (opt (string "$"))
              (regexp "[A-Z]+")
              (opt (string "$"))
              (regexp "[0-9]+"))

   ;; ’ ([A-Z0-9_ !@#$%^&*()+={}:;|<>,./?\\] | ”)+ ’
   :DDECALL (cat (string "'")
                 (alt (regexp "[A-Z0-9_ \\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\+\\=\\{\\}\\:\\;\\|\\<\\>\\,\\.\\/\\?\\\\]+")
                      (string "\""))
                 (string "'"))

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
   :FILE (cat (hide (string "["))
              (regexp "[0-9]+")
              (hide (string "]")))

   ;; (Any entry from the function list1) \(
   :FUNCTION (cat (apply alt (map string function-list))
                  (hide (string "(")))

   ;; $? [0-9]+ : $? [0-9]+
   :HORIZONTAL-RANGE (regexp "$? [0-9]+ : $? [0-9]+")

   ;; [A-Z0-9]+ : ([A-Z0-9_.]+ | ’ ([A-Z0-9_ !^&*()+={}:;|<>,./?\\] | ”)+ ’) !
   :MULTIPLE-SHEETS
   (let [initial (regexp "[A-Za-z0-9]+")
         colon (hide (string ":"))
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
   :NAMED-RANGE (regexp "[A-Za-z_][A-Za-z0-9_.]*")

   ;; (TRUE | FALSE | [A-Z]+[0-9]+) [A-Z0-9_.]+
   :NAMED-RANGE-PREFIXED (cat (alt (string "TRUE")
                                   (string "FALSE")
                                   (cat (regexp "[A-Za-z]+")
                                        (regexp "[0-9]+")))
                              (regexp "[A-Za-z0-9_\\.]+"))

   ;; [0-9]+ ,? [0-9]* (e [0-9]+)?
   :NUMBER (cat (regexp "[0-9]+")
                (opt (alt (string ",")
                          (string ".")))
                (regexp "[0-9]*")
                (opt (cat (string "e") (regexp "[0-9]+"))))

   ;; ’\[ [0-9]+ \] ([0-9A-Z_ !@#$%^&*()+={}:;|<>,./?\\] | ”)+ ’!
   :QUOTED-FILE-SHEET (let [big-regexp
                            (regexp #"([0-9A-Za-z_\- \\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\+\\=\\|\\:\\;\\<\\>\\,\\.\\?\\\\\\\"])+")]

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
   :SHEET (let [big-regexp (regexp #"([0-9A-Za-z_\-\ \\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\+\\=\\|\\:\\;\\<\\>\\,\\.\\?\\\\\\\"])+")
                single-quote (string "'")]

            (cat (alt (regexp "[0-9A-Za-z_\\.]+")
                      (cat (hide single-quote)
                           (plus (alt big-regexp
                                      (string "\"")))
                           (hide single-quote)))
                 (string "!")))

   ;; " ([^ "]|"")* "
   :STRING (cat (hide (string "\""))
                (regexp "([^\"])*|\"\"")
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
