
(ert-deftest nom/parse-a-simple-classfile ()
  (should (equal (nom/parse-file "Class1.java")
                 '(("Class1" (:type :class) (:bounds 36 61))))))

(ert-deftest nom/parse-a-more-difficult-classfile ()
  (should (equal (nom/parse-file "Class2.java")
                 '(("Class2" (:type :class) (:bounds 80 280) 
                    (:inner (("MyEnum" (:type :enum) (:bounds 118 132)) 
                             ("MyComparator" (:type :class) (:bounds 198 276) (:implements "Comparator")))))))))

(ert-deftest nom/parse-classfile-with-several-toplevel-classes ()
  (should (equal (nom/parse-file "Class3.java")
                 '(("Class3" (:type :class) (:bounds 36 41))
                   ("NonPublic" (:type :class) (:bounds 59 64))))))
