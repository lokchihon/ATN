(ns grammar.grammar
  (require [grammar.parser :refer :all]))

;;STATES
(s/_
  (push np/_ test
        (setr subj *)
        (setrq type dcl)
        (to s/np))
  (jump s/np
        (setrq type q)))

(s/np
  (cat v (getf tns)
       (setr v *)
       (setr tns (buildq ("tns" #)(getf tense)))
             (pncode (getf pncode))
             (to s/aux)))

(s/aux
  (cat neg (nullr neg)
       (setrq neg (neg))
       (cond ((= (getr v) (quote do))
              (setrq v nil)))
       (to s/aux))
  (jump vp/v (and (getr subj)
                  (agree (getr subj) (getr pncode))))
  (push np/_ (nullr subj)
        (cond ((not (agree * (getr pncode)))
               (abort)))
        (setr subj *)
        (to vp/v)))

(np/_
  (cat art test
       (setr art (buildq (("art" *))))
       (to np/art))
  (jump np/art test))

(np/adj
  (cat n test
       (setr n *)
       (setr nu (getf number))
       (to np/n))
  (cat n test
       (addl adjs (buildq ("adj" ("np" ("n" *) ("nu" #)))
                            (getf number)))
             (to np/adj)))

(np/art
  (cat quant test
       (setr quant (buildq (("quant" *))))
       (to np/quant))
  (jump np/quant t))

(np/quant
  (cat adj test
       (addr adjs (buildq (@ ("adj") # (*)) (getf degree)))
       (to np/quant))
  (jump np/adj t))

(np/n
  (push pp/_ (ppstart)
        (addl nmods *)
        (to np/n))
  (pop (buildq (@ ("np") + + + (("n" +)) (("nu" +)) +)
               art quant adjs n nu nmods)
       (detagree)))

(pp/_
  (cat prep test
       (setr prep *)
       (to pp/prep)))

(pp/prep
  (push np/_ (npstart)
        (setr np *)
        (to pp/np)))

(pp/np
  (pop (buildq ("pp" ("prep" +) +) prep np)
       test))

(vp/v
  (cat v (and (getf pastpart)
              (= (getr v) (quote be)))
       (hold (quote np) (getr subj))
       (setrq subj (np (pro someone)))
       (setr agflag t)
       (setr v *)
       (to vp/v))
  (cat v (and (getf pastpart)
              (= (getr v) (quote have)))
       (addr tns (quote perfect))
       (setr v *)
       (to vp/v))
  (cat v (and (getf untensed)
              (setr modal)
              (nullr v))
       (setr v *)
       (to vp/v))
  (cat v (and (getf prespart)
              (= (getr v) (quote be)))
       (addr tns (quote progressive))
       (setr v *)
       (to vp/v))
  (jump vp/head test
        (cond ((or (getr modal) (getr neg))
               (setr aux (buildq ((@ ("aux") + +)) modal neg))))))

(vp/head
  (jump vp/vp (getf intrans (getr v)))
  (push np/_ (getf trans (getr v))
        (setr obj *)
        (to vp/obj))
  (vir np (getf trans (getr v))
       (setr obj *)
       (to vp/obj))
  (wrd to (and (getf scomp (getr v))
               (nullr agflag))
       (setr specialsubj (getr obj))
       (to vp/tp)))

(vp/obj
  (jump vp/vp test)
  (wrd to (getf scomp (getr v))
       (setr specialsubj (getr obj))
       (to vp/to)))

(vp/vp
  (push pp/_ test
        (addr vmods *)
        (to vp/vp))
  (wrd by (getr agflag)
       (setr agflag nil)
       (to vp/by))
  (pop (cond ((getr obj) (buildq ("s" + + + (@ + ("vp" ("v" +) +) +))
                                 type subj tns aux v obj vmods))
             (test (buildq ("s" + + + (@ + ("vp" ("v" +)) +))
                           type subj tns aux v vmods)))))

(vp/by
  (push np/_ test
        (setr subj *)
        (to vp/vp)))

(vp/to
  (push vp/_ test
        (sendr subj (getr specialsubj))
        (sendr tns (tenseof (getr tns)))
        (sendrq type comp)
        (setr obj *)
        (to vp/vp)))

(comp/_
  (cat v (getf untensed)
       (setr v *)
       (to vp/v)))