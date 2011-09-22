; Code from previous chapters
(define +1 (x) (+ x 1))
(define or (x y) (if x x y))
(define and (x y) (if x y x))
(define not (x) (if x false true))
(define <> (x y) (not (= x y)))
(define <= (x y) (or (< x y) (= x y)))
(define divides (n x) (= (* n (/ x n)) x))
(define mod (n m) (- n (* m (/ n m))))
(define gcd (i j)
   (if (> i j) (gcd j i)
     (if (divides i j) i (gcd (mod j i) i))))
(define abs (x) (if (< x 0) (- 0 x) x))
; Section 7.1.1
(class FinancialHistory Object
    (cashOnHand incomes expenditures)
    (define initFinancialHistory (amount)
         (begin
              (set cashOnHand amount)
              (set incomes (mkDictionary))
              (set expenditures (mkDictionary))
              self))
    (define receive:from: (amount source)
         (begin
            (at:put: incomes source (+ (totalReceivedFrom: self source) amount))
            (set cashOnHand (+ cashOnHand amount))))
    (define spend:for: (amount reason)
         (begin
           (at:put: expenditures reason (+ (totalSpentFor: self reason) amount))
           (set cashOnHand (- cashOnHand amount))))
    (define cashOnHand () cashOnHand)
    (define totalReceivedFrom: (source)
         (if (includesKey: incomes source)
              (at: incomes source)
              0))
    (define totalSpentFor: (reason)
         (if (includesKey: expenditures reason)
              (at: expenditures reason)
              0))
)
(define mkFinancialHistory (amount)
     (initFinancialHistory (new FinancialHistory) amount))
; Test cases for FinancialHistory moved to after Collection hierarchy
(class DeductibleHistory FinancialHistory
    (deductible)
    (define initDeductibleHistory (amount)
         (begin
              (initFinancialHistory self amount)
              (set deductible 0)
              self))
    (define spend:Deduct: (amount reason)
         (begin
              (spend:for: self amount reason)
              (set deductible (+ deductible amount))))
    (define spend:for:deduct: (amount reason deduction)
         (begin
              (spend:for: self amount reason)
              (set deductible (+ deductible deduction))))
    (define totalDeductions () deductible))
(define mkDeductibleHistory (amount)
     (initDeductibleHistory (new DeductibleHistory) amount))
; Test cases for DeductibleHistory moved to after Collection hierarchy
; Section 7.1.5
(set false 0)
(set true 1)
(define isNil (x) (= x #nil))
(define notNil (x) (<> x #nil))
(class Collection Object
    ()  ; abstract class
    (define first () #subclassResponsibility)
    (define next () #subclassResponsibility)
    (define add: (item) #subclassResponsibility)
    (define size ()
         (begin
              (set tempitem (first self))
              (set tempsize 0)
              (while (notNil tempitem)
                   (begin
                        (set tempsize (+1 tempsize))
                        (set tempitem (next self))))
              tempsize))
    (define isEmpty () (isNil (first self)))
    (define includes: (item)
         (begin
              (set tempitem (first self))
              (set found false)
              (while (and (notNil tempitem) (not found))
                   (if (= tempitem item)
                        (set found true)
                        (set tempitem (next self))))
              found))
)
(class Set Collection
    (members)  ; list of elements
    (define initSet () (begin (set members (mkList)) self))
    (define first () (first members))
    (define next () (next members))
    (define add: (item)
         (if (includes: members item) self (add: members item)))
)
(define mkSet () (initSet (new Set)))
;
(class KeyedCollection Collection
    ()  ; abstract class
    (define at:put: (key value) #subclassResponsibility)
    (define currentKey () #subclassResponsibility)
    (define at: (key)
         (begin
              (set tempvalue (first self))
              (set found false)
              (while (and (notNil tempvalue) (not found))
                   (if (= key (currentKey self))
                        (set found true)
                        (set tempvalue (next self))))
              tempvalue))  ; note: nil if key out of range
    (define includesKey: (key)
         (notNil (at: self key)))
    (define indexOf: (value)
         (begin
              (set tempvalue (first self))
              (set found false)
              (while (and (notNil tempvalue) (not found))
                   (if (= value tempvalue)
                        (set found true)
                        (set tempvalue (next self))))
              (if (isNil tempvalue) #nil (currentKey self))))
)
;
(class Association Object 
   (fst snd)
     (define initAssociation (x y) (begin (set fst x) (set snd y) self))
     (define fst () fst)
     (define snd () snd)
     (define fst:  (x) (set fst x))
     (define snd:  (y) (set snd y))
)
;
(define mkAssociation (a b) (initAssociation (new Association) a b))
(class Dictionary KeyedCollection
    (table currentKey)
    (define initDictionary ()
         (begin (set table (mkList)) self))
    (define currentKey () currentKey)
    (define first ()
         (if (isEmpty table) #nil
              (begin
                   (set tempassoc (first table))
                   (set currentKey (fst tempassoc))
                   (snd tempassoc))))
    (define next ()
         (begin
              (set tempassoc (next table))
              (if (isNil tempassoc) #nil
                   (begin
                        (set currentKey (fst tempassoc))
                        (snd tempassoc)))))
    (define at:put: (key value)
         (begin
              (set tempassoc (associationAt: self key))
              (if (isNil tempassoc)
                   (add: table (mkAssociation key value))
                   (snd: tempassoc value))))
    (define associationAt: (key)
         (begin
              (set temptable table)
              (set found false)
              (while (not (or (isEmpty temptable) found))
                   (if (= (fst (car temptable)) key)
                       (set found true)
                       (set temptable (cdr temptable))))
              (if found (car temptable) #nil)))
)
(define mkDictionary () (initDictionary (new Dictionary)))
;
(class SequenceableCollection KeyedCollection
    () ; abstract class
    (define firstKey () #subclassResponsibility)
    (define lastKey () #subclassResponsibility)
    (define last () (at:  self (lastKey self)))
   (define at: (index)
         (begin
               (set iterations (- index (firstKey self)))
               (set result (first self))
               (while (> iterations 0)
                     (begin
                           (set result (next self))
                           (set iterations (- iterations 1))))
               result))
)
(class List SequenceableCollection
    (car cdr currentKey currentCell)
    (define car () car)
    (define cdr () cdr)
    (define initList ()
         (begin (set car #nil) self))
    (define add: (item)
         (begin
              (set temp (new List))
              (car: temp car)
              (cdr: temp cdr)
              (set cdr temp)
              (set car item)))
    (define car: (x) (set car x))
    (define cdr: (x) (set cdr x))
    (define first ()
         (begin
              (set currentKey 1)
              (set currentCell self)
              car))
    (define next ()
         (if (isNil (car currentCell)) #nil
              (begin
                   (set currentKey (+1 currentKey))
                   (set currentCell (cdr currentCell))
                   (car currentCell))))
    (define firstKey () 1)
    (define lastKey () (size self))
    (define currentKey () currentKey)
    (define at:put: (n value)
         (if (= n 1) (set car value)
             (at:put: cdr (- n 1) value)))
    (define removeFirst ()
         (if (isEmpty self) self ; do nothing
              (begin
                    (set car (car cdr))
                    (set cdr (cdr cdr)))))
    (define zerolist (size)
            (while (> size 0)
                 (begin
                      (add: self 0)
                      (set size (- size 1)))))
)
(define mkList () (initList (new List)))
;
(class Array SequenceableCollection
    (elements lobound hibound currentKey)
    (define initArray (lo size)
         (begin
              (set lobound lo)
              (set hibound (- (+ lo size) 1))
              (set elements (new List))
              (zerolist elements size)
              self))
    (define size () (+1 (- hibound lobound)))
    (define firstKey () lobound)
    (define lastKey () hibound)
    (define currentKey () currentKey)
    (define first ()
         (begin
              (set currentKey lobound)
              (first elements)))
    (define next ()
         (if (= currentKey hibound) #nil
              (begin
                   (set currentKey (+1 currentKey))
                (next elements))))
    (define at:put: (n value)
         (if (> n hibound) #nil
             (if (< n lobound) #nil
                  (at:put: elements (+1 (- n lobound)) value))))
)
(define mkArray (l s) (initArray (new Array) l s))
; Test cases for FinancialHistory and DeductibleHistory
(set myaccount (mkFinancialHistory 1000))
(spend:for: myaccount 50 #insurance)
(receive:from: myaccount 200 #salary)
(cashOnHand myaccount)
1150
(spend:for: myaccount 100 #books)
(cashOnHand myaccount)
1050
(set myaccount (mkDeductibleHistory 1000))
(spend:for: myaccount 50 #insurance)
(receive:from: myaccount 200 #salary)
(cashOnHand myaccount)
1150
(totalDeductions myaccount)
0
(spend:Deduct: myaccount 100 #mortgage)
(cashOnHand myaccount)
1050
(totalDeductions myaccount)
100
(spend:for:deduct: myaccount 100 #3-martini-lunch 50)
(cashOnHand myaccount)
950
(totalDeductions myaccount)
150
; Section 7.3
(class Number Object
    ()  ; abstract class
    (define + (x) #subclassResponsibility)
    (define negate () #subclassResponsibility)
    (define * (x) #subclassResponsibility)
    (define recip () #subclassResponsibility)
    (define = (x) #subclassResponsibility)
    (define < (x) #subclassResponsibility)
    (define zero () #subclassResponsibility)
    (define one () #subclassResponsibility)
    (define print () #subclassResponsibility)
    (define - (y) (+ self (negate y)))
    (define / (y) (* self (recip y)))
    (define > (y) (< y self))
    (define +1 () (+ self (one self)))
    (define sub1 () (- self (one self)))
    (define isZero () (= self (zero self)))
    (define isNegative () (< self (zero self)))
    (define abs () (if (isNegative self) (negate self) self))
    (define sqr () (* self self))
    (define sqrt (epsilon) ; find square root of receiver within epsilon
         (begin
              (set this-step (+1 (zero self)))
              (set two (+1 this-step))
              (set next-step (/ (+ this-step (/ self this-step)) two))
              (while (> (abs (- this-step next-step)) epsilon)
                   (begin
                        (set this-step next-step)
                        (set next-step (/
                            (+ this-step (/ self this-step)) two))))
              next-step))
)
(class Fraction Number
    (x y)
    (define initFraction (a b)
         (begin
               (setFraction self a b)
               (sign-reduce self)
               (div-reduce self)))
    (define setFraction (a b) (begin (set x a) (set y b) self))
    (define x () x)
    (define y () y)
    (define + (f)
         (div-reduce
            (setFraction (new Fraction)
                            (+ (* x (y f)) (* (x f) y))
                            (* y (y f)))))
    (define negate () (setFraction (new Fraction) (- 0 x) y))
    (define * (f)
         (div-reduce
              (setFraction (new Fraction)
                            (* x (x f))
                            (* y (y f)))))
    (define recip ()
     (sign-reduce (setFraction (new Fraction) y x)))
    (define = (f)
     (and (= x (x f)) (= y (y f))))
    (define < (f)
     (< (* x (y f)) (* (x f) y)))
    (define zero () (setFraction (new Fraction) 0 1))
    (define one () (setFraction (new Fraction) 1 1))
    (define print () (begin (print x) (print y)))
    ; div-reduce and sign-reduce should not be exported
    (define div-reduce ()
         (begin
              (if (= x 0)
                   (set y 1)
                   (begin
                         (set temp (gcd (abs x) y))
                         (set x (/ x temp))
                         (set y (/ y temp))))
              self))
    (define sign-reduce ()
         (begin
              (if (< y 0)
                   (begin (set x (- 0 x))(set y (- 0 y)))
                   0)
              self))
)
(define mkFraction (a b) (initFraction (new Fraction) a b))
(set eps (mkFraction 1 2))
(set f1 (mkFraction 17 1))
(set f2 (sqrt f1 eps))
(print f2)
 3437249
 833049
;
(class Float Number
    (mant exp)
    (define initFloat (m e)
     (begin (set mant m) (set exp e) self))
    (define mant () mant)
    (define exp () exp)
    (define + (x)
         (begin
              (if (< exp (exp x))
                  (begin
                       (set min self)
                       (set max x))
                  (begin
                       (set min x)
                       (set max self)))
              (set delta (- (exp max) (exp min)))
              (set temp (+ (* (mant max) (powerof10 self delta)) (mant min)))
              (normalize
                  (initFloat (new Float) temp (if (= temp 0) 0 (exp min))))))
    (define negate ()  (initFloat (new Float) (- 0 mant) exp))
    (define * (x)
         (normalize (initFloat (new Float)
              (* mant (mant x))
              (+ exp (exp x)))))
    (define recip ()
         (if (isZero self) self
             (normalize (initFloat (new Float)
                  (/ 100000000 mant)
                  (- (- 0 8) exp)))))
    (define zero () (initFloat (new Float) 0 0))
    (define one () (initFloat (new Float) 1 0))
    (define = (x) (isZero (- self x)))
    (define < (x) (isNegative (- self x)))
    (define print () (begin (print mant) (print exp)))
    (define isZero () (= mant 0))
    (define isNegative () (< mant 0))
    ; normalize and powerof10 should not be exported
    (define powerof10 (d)
     (if (= d 0) 1 (* 10 (powerof10 self (- d 1)))))
    (define normalize ()
           (begin
                (while (> (abs mant) 10000)
                     (begin
                          (set mant (/ mant 10))
                          (set exp (+ exp 1))))
                self))
)
(define mkFloat (m e) (initFloat (new Float) m e))
(set eps (mkFloat 5 -1))
(set x1 (mkFloat 17 0))
(print (sqrt x1 eps))
4125
-3
; Section 7.4
(class Simulation Object
     () ; abstract class
     (define initialize () #subclassResponsibility)
     (define report () #subclassResponsibility)
     (define run (timelimit)
          (begin
                (set CLOCK 0)
                (set EVQUEUE (mkEventQueue))
                (initialize self)
                (while (<= CLOCK timelimit) (doNextEvent EVQUEUE))
                (report self)))
)
(class LabSimulation Simulation
     (termlimit)
     (define initLabSimulation (t)
           (begin (set termlimit t) self))
     (define initialize ()
          (begin
                (set TERMINALLIMIT termlimit)
                (set THELAB (mkLab))
                (set STUDENTNO 0)
                (set STUDENTSFINISHING 0)
                (set TOTALTIMEWASTED 0)
                (set TERMINALQUEUE (mkQueue))
                (set WAITTIMES (mkWaitTimeList))
                (set SERVICETIMES (mkServiceTimeList))
                (scheduleNewArrival (new Student))))
     (define report ()
          (begin
                (print #simulation-done)
                (print #students-finishing)
                (print STUDENTSFINISHING)
                (print #left-on-queue)
                (print (size TERMINALQUEUE))
                (print #Total-time-wasted:)
                (print TOTALTIMEWASTED)
                (print #Average-time-wasted:)
                (print (/ TOTALTIMEWASTED STUDENTSFINISHING))))
)
(define mkLabSimulation (tl) (initLabSimulation (new LabSimulation) tl))
(class Lab Object
    (term1free term2free)
    (define initLab ()
           (begin (set term1free true) (set term2free true) self))
    (define terminals-free? () (or term1free term2free))
    (define pick-terminal ()
         (if term1free
              (begin (set term1free false) 1)
              (begin (set term2free false) 2)))
    (define release-terminal (t)
         (if (= t 1) (set term1free true) (set term2free true)))
)
(define mkLab () (initLab (new Lab)))
;
(class List SequenceableCollection
  (car cdr currentKey currentCell)
  (define car () car)
  (define cdr () cdr)
  (define initList ()
     (begin (set car #nil) self))
  (define newEmptyCollection () (initList (new List)))
  (define add: (item)
     (begin
        (set temp (newEmptyCollection self))
        (car: temp car)
        (cdr: temp cdr)
        (set cdr temp)
        (set car item)))
  (define car: (x) (set car x))
  (define cdr: (x) (set cdr x))
  (define first ()
     (begin
        (set currentKey 1)
        (set currentCell self)
        car))
  (define next ()
     (if (isNil (car currentCell)) #nil
        (begin
           (set currentKey (+1 currentKey))
           (set currentCell (cdr currentCell))
           (car currentCell))))
  (define firstKey () 1)
  (define lastKey () (size self))
  (define currentKey () currentKey)
  (define at:put: (n value)
     (if (= n 1) (set car value)
       (at:put: cdr (- n 1) value)))
  (define removeFirst ()
     (if (isEmpty self) self ; do nothing
        (begin
            (set car (car cdr))
            (set cdr (cdr cdr)))))
  (define zerolist (size)
        (while (> size 0)
           (begin
              (add: self 0)
              (set size (- size 1)))))
)
;
(class Queue List
     ()
    (define initQueue () (initList self))
    (define newEmptyCollection () (initList (new Queue)))
    (define enqueue: (item)
         (if (isEmpty self) (add: self item) (enqueue: cdr item)))
)
(define mkQueue () (initQueue (new Queue)))
(class EventQueue Object
    (pqueue) ; a PriorityQueue
    (define initEventQueue ()
         (begin (set pqueue (mkPriorityQueue)) self))
    (define scheduleEvent (event time)
         (insert: pqueue (mkAssociation time event)))
    (define doNextEvent ()
         (begin
               (set pair (car pqueue))
               (removeFirst pqueue)
               (set CLOCK (fst pair))
               (takeAction (snd pair))))
)
(define mkEventQueue () (initEventQueue (new EventQueue)))
(class PriorityQueue List
    ()
    (define initPriorityQueue () (initList self))
    (define newEmptyCollection () (initList (new PriorityQueue)))
    (define insert: (pair)
        (if (isEmpty self) (add: self pair)
             (if (< (fst pair) (fst car)) (add: self pair)
                  (insert: cdr pair))))
)
(define mkPriorityQueue () (initPriorityQueue (new PriorityQueue)))
(class Student Object
    (status number timeNeeded timeStillNeeded arrivalTime)
    (define initStudent (n t a)
         (begin
               (set status -1)
               (set number n)
               (set timeNeeded t)
               (set timeStillNeeded t)
               (set arrivalTime a)
               self))
    (define takeAction ()
         (if (= status -1) (arrive self) (leaveTerminal self)))
    (define arrive ()
         (begin
               (if (terminals-free? THELAB)
                    (grabTerminal self)
                    (begin
                          (set status 0)
                          (enqueue: TERMINALQUEUE self)))
               (scheduleNewArrival (new Student))))
    (define leaveTerminal ()
         (if (= timeStillNeeded 0)
              (begin
                    (release-terminal THELAB status)
                    (set status 3)
            (set wasted (- (- CLOCK arrivalTime) timeNeeded))
                 (set STUDENTSFINISHING (+1 STUDENTSFINISHING))
                 (set TOTALTIMEWASTED (+ TOTALTIMEWASTED wasted))
                    (if (not (isEmpty TERMINALQUEUE))
                         (grabTerminal (car TERMINALQUEUE)) 0))
              (if (isEmpty TERMINALQUEUE)
                   (scheduleLeaveTerminal self)
                   (begin
                         (release-terminal THELAB status) ; bug in book
                         (set status 0)
                         (enqueue: TERMINALQUEUE self)
                         (grabTerminal (car TERMINALQUEUE))))))
    (define grabTerminal ()
         (begin
               (if (= status 0) ; was on terminal queue
                    (removeFirst TERMINALQUEUE)
                     0) ; else do nothing
               (set status (pick-terminal THELAB))
               (scheduleLeaveTerminal self)))
    (define scheduleLeaveTerminal ()
         (if (<= timeStillNeeded TERMINALLIMIT)
              (begin
                    (scheduleEvent EVQUEUE self (+ CLOCK timeStillNeeded))
                    (set timeStillNeeded 0))
              (begin
                  (scheduleEvent EVQUEUE self (+ CLOCK TERMINALLIMIT))
                  (set timeStillNeeded (- timeStillNeeded TERMINALLIMIT)))))
    (define scheduleNewArrival ()
         (begin
               (set wait (next WAITTIMES))
               (set service (next SERVICETIMES))
               (set STUDENTNO (+1 STUDENTNO))
               (set arrival (+ CLOCK wait))
               (initStudent self STUDENTNO service arrival)
               (scheduleEvent EVQUEUE self arrival)))
)
; Twenty arrivals at time zero
(class WaitTimeList Object
    (which)
    (define initWaitTimeList () (begin (set which 0) self))
    (define next ()
         (if (= which 20) 2000 (begin (set which (+1 which)) 0)))
)
(define mkWaitTimeList () (initWaitTimeList (new WaitTimeList)))
; All students need 120 minutes of terminal time
(class ServiceTimeList Object
    ()
    (define initServiceTimeList () self)
    (define next () 120)
)
(define mkServiceTimeList () (initServiceTimeList (new ServiceTimeList)))
(set sim30 (mkLabSimulation 30))
(run sim30 1200)
#simulation-done
#students-finishing
20
#left-on-queue
0
#Total-time-wasted:
18900
#Average-time-wasted:
945
; Arrivals every 30 minutes
(class WaitTimeList Object
    ()
    (define initWaitTimeList () self)
    (define next () 30)
)
(define mkWaitTimeList () (initWaitTimeList (new WaitTimeList)))
; Service times alternating: 120, 30, 120, 30, ...
(class ServiceTimeList Object
    (which)
    (define initServiceTimeList () (begin (set which 1) self))
    (define next ()
          (begin
                (set which (- 1 which))
                (if (= which 0) 120 30)))
)
(define mkServiceTimeList () (initServiceTimeList (new ServiceTimeList)))
(set sim30 (mkLabSimulation 30))
(run sim30 1200)
#simulation-done
#students-finishing
30
#left-on-queue
8
#Total-time-wasted:
3090
#Average-time-wasted:
103
quit
