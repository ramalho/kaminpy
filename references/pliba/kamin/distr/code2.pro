; Section 8.4 - Note that two versions of transform are given.
; This file can only be run if one of these is commented out.
; Currently, the first, and less efficient, version is commented out.
(infer (member X (cons X L)))
(infer (member X (cons Y M)) from (member X M))
;
(infer (block a))
(infer (block b))
(infer (block c))
;
(infer (different a b))
(infer (different a c))
(infer (different b a))
(infer (different b c))
(infer (different c a))
(infer (different c b))
(infer (different X table) from (block X))
(infer (different table Y) from (block Y))
;
(infer (clear X nil))
(infer (clear X (cons (on B Y) State))
      from (different X Y) (clear X State))
;
(infer (on X Y State) from (member (on X Y) State))
;
(infer (update (move X Y Z) (cons (on X Y) S) (cons (on X Z) S)))
(infer (update (move X Y Z) (cons (on U V) S1) (cons (on U V) S2))
      from (different X U) (update (move X Y Z) S1 S2))
;
(infer (legal-move (move B P1 table) State)
      from
             (on B P1 State)
             (different P1 table)
             (clear B State))
(infer (legal-move (move B1 P B2) State)
      from
             (block B2)
             (on B1 P State)
             (different P B2)
             (different B1 B2)
             (clear B1 State)
             (clear B2 State))
;
(infer (different (cons (on A X) State1) (cons (on A Y) State2))
      from (different X Y))
(infer (different (cons (on A X) State1) (cons (on A X) State2))
      from (different State1 State2))
;
(infer (not-member X nil))
(infer (not-member X (cons Y L))
      from (different X Y) (not-member X L))
;
;(infer (transform State1 State2 Plan)
;      from (transform State1 State2 (cons State1 nil) Plan))
;;
;(infer (transform State State Visited nil))
;(infer (transform State1 State2 Visited (cons Move Moves))
;      from
;            (legal-move Move State1)
;             (update Move State1 State)
;             (not-member State Visited)
;             (transform State State2 (cons State Visited) Moves))
;;
;(infer (state1 (cons (on a b) (cons (on b table) (cons (on c a) nil)))))
;(infer (state2 (cons (on a b) (cons (on b c) (cons (on c table) nil)))))
;(infer? (state1 S1) (state2 S2) (transform S1 S2 Plan)
;              (print Plan))
;(infer? (print (cons (move c a table)
;   (cons (move a b table) (cons (move b table a)
;   (cons (move b a c) (cons (move a table b) nil)))))))
;
(infer (transform State1 State2 Plan)
      from (transform State1 State2 (cons State1 nil) Plan))
;
(infer (transform State State Visited nil))
(infer (transform State1 State2 Visited (cons Move Moves))
      from
             (choose-move Move State1 State2)
             (update Move State1 State)
             (not-member State Visited)
             (transform State State2 (cons State Visited) Moves))
;
(infer (choose-move Move State1 State2)
      from (suggest Move State2) (legal-move Move State1))
(infer (choose-move Move State1 State2)
      from (legal-move Move State1))
(infer (suggest (move X Y Z) State)
      from (member (on X Z) State))
(infer (state1 (cons (on a b) (cons (on b table) (cons (on c a) nil)))))
(infer (state2 (cons (on a b) (cons (on b c) (cons (on c table) nil)))))
(infer? (state1 S1) (state2 S2) (transform S1 S2 Plan)
              (print Plan))
(infer? (print (cons (move c a table) (cons (move a b table) (cons (move b table c)
   (cons (move a table b) nil))))))
quit
