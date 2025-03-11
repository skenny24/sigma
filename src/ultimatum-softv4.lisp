(in-package :sigma)
(defun ultimatum-soft ()
  (init nil '(a b))
;  (setq default-integral nil)

  (new-type 'quantity :numeric t :discrete t :min 0 :max 4)

  (predicate 'offer :world 'open :arguments '((agent agent) (quantity quantity %)))
  (predicate 'money :world 'open :arguments '((agent agent) (quantity quantity %)))
  (predicate 'operator-a :world 'open :arguments '((quantity quantity)))
  (predicate 'operator-b :world 'open :arguments '((offer quantity) (acceptance boolean)))
  (predicate 'operator-b-e :world 'open :arguments '((offer quantity) (acceptance boolean ^)))

  (conditional 'transition-a
               :conditions '((offer (agent a) (quantity (offer)))
                             )
               :condacts '((operator-a (quantity (offer)))))

  (conditional 'transition-b-b
               :conditions '((money (agent b) (quantity (moneyb))))
               :condacts '((operator-b (offer (offer)) (acceptance (choice)))
                           )
               :function-variable-names '(choice offer moneyb)
               :function '((1 true  0 0)
                           (1 true  1 1)
                           (1 true  2 2)
                           (1 true  3 3)
                           (1 false * 0))
               )

  (conditional 'exponentiate
               :conditions '((operator-b (offer (offer)) (acceptance (choice))))
               :actions '((operator-b-e (offer (offer)) (acceptance (choice))))
               )

  (conditional 'transition-a-b
               :conditions '((money (agent a) (quantity (moneya)))
                             (operator-b-e (offer (offer)) (acceptance (choice)))
                             )
               :condacts '((offer (agent a) (quantity (offer))))
               :function-variable-names '(choice offer moneya)
               :function '((1 true  0 3)
                           (1 true  1 2)
                           (1 true  2 1)
                           (1 true  3 0)
                           (1 false * 0))
               )

  (conditional 'reward
               :condacts '((money (agent (a)) (quantity (n))))
               :function-variable-names '(a n)
               :function '((.1 a 0) (.4 a 1) (.7 a 2) (1 a 3)
                           (.1 b 0) (.4 b 1) (.7 b 2) (1 b 3))
               )

  (when open-world-wmfns
    (evidence '((operator-b-e .01 (offer *) (acceptance *))))
    )

  (d 0)
  (pwm 'array)
;  (setq open-conditions-like-condacts nil)
  )

(defun test-ultimatum-soft ()
  (ultimatum-soft)
  (ppwm 'offer 'array info-stream)
  (print-global-decision-statistics info-stream)
  )