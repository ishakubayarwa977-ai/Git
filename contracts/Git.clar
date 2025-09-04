;; winning-brief.clar
;; Compact winning RFP: commit-reveal, evaluator voting, badge
(clarity-version 2)

;; Errors
(define-constant ERR-RFP-NOT-FOUND (err u100))
(define-constant ERR-BAD-TIME (err u101))
(define-constant ERR-NO-COMMIT (err u102))
(define-constant ERR-HASH-MISMATCH (err u103))
(define-constant ERR-NOT-EVAL (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-NO-CANDIDATES (err u106))

;; Status
(define-constant S_COMMIT u1)
(define-constant S_REVEAL  u2)
(define-constant S_EVALUATE u3)
(define-constant S_FINAL   u4)

;; Counters
(define-data-var next-rfp uint u0)
(define-data-var next-badge uint u0)

;; RFP store
(define-map rfps ((id uint))
  ((owner principal) (title (string-ascii 64)) (commit-dead uint) (reveal-dead uint) (eval-dead uint) (status uint) (winner (optional principal))))

;; commits: (id, vendor) -> buff32
(define-map commits ((id uint) (vendor principal)) ((hash (buff 32)) (committed-at uint)))

;; proposals: (id, vendor) -> (uri, revealed-at)
(define-map proposals ((id uint) (vendor principal)) ((uri (string-utf8 200)) (revealed-at uint)))

;; evaluator allowlist per rfp
(define-map evaluators ((id uint) (who principal)) ((allowed bool)))

;; votes: (id, evaluator, vendor) -> score
(define-map votes ((id uint) (evaluator principal) (vendor principal)) ((score uint)))

;; tallies: (id, vendor) -> (sum, count)
(define-map tallies ((id uint) (vendor principal)) ((sum uint) (count uint)))

;; badges: badge-id -> (owner, meta)
(define-map badges ((bid uint)) ((owner principal) (meta (string-utf8 120))))

;; Create RFP
(define-public (create-rfp (title (string-ascii 64)) (commit-dead uint) (reveal-dead uint) (eval-dead uint))
  (begin
    (asserts! (> commit-dead block-height) ERR-BAD-TIME)
    (asserts! (> reveal-dead commit-dead) ERR-BAD-TIME)
    (asserts! (> eval-dead reveal-dead) ERR-BAD-TIME)
    (let ((id (+ (var-get next-rfp) u1)))
      (map-set rfps { id: id } { owner: tx-sender, title: title, commit-dead: commit-dead, reveal-dead: reveal-dead, eval-dead: eval-dead, status: S_COMMIT, winner: none })
      (var-set next-rfp id)
      (ok id))))

;; Vendor commit: sha256(id || hash160(vendor) || utf8(uri) || salt)
(define-public (commit (id uint) (h (buff 32)))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (asserts! (<= block-height (get commit-dead rfp)) ERR-BAD-TIME)
             (map-set commits { id: id, vendor: tx-sender } { hash: h, committed-at: block-height })
             (ok true))
      none ERR-RFP-NOT-FOUND)))

;; Vendor reveal
(define-public (reveal (id uint) (uri (string-utf8 200)) (salt (buff 32)))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (asserts! (> block-height (get commit-dead rfp)) ERR-BAD-TIME)
             (asserts! (<= block-height (get reveal-dead rfp)) ERR-BAD-TIME)
             (let ((c (map-get? commits { id: id, vendor: tx-sender })))
               (match c
                 somec (let ((stored (get hash (unwrap! somec somec)))
                             (computed (sha256 (concat (to-buff id) (concat (to-buff (hash160 tx-sender)) (concat (utf8-to-bytes uri) salt))))))
                         (asserts! (is-eq stored computed) ERR-HASH-MISMATCH)
                         (map-set proposals { id: id, vendor: tx-sender } { uri: uri, revealed-at: block-height })
                         (ok true))
                 none (err u102))))
      none ERR-RFP-NOT-FOUND)))

;; Owner adds evaluator
(define-public (add-evaluator (id uint) (who principal))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (asserts! (is-eq (get owner rfp) tx-sender) ERR-RFP-NOT-FOUND)
             (map-set evaluators { id: id, who: who } { allowed: true })
             (ok true))
      none ERR-RFP-NOT-FOUND)))

;; Start evaluation (owner)
(define-public (start-eval (id uint))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (asserts! (is-eq (get owner rfp) tx-sender) ERR-RFP-NOT-FOUND)
             (asserts! (> block-height (get reveal-dead rfp)) ERR-BAD-TIME)
             (map-set rfps { id: id } (merge rfp { status: S_EVALUATE }))
             (ok true))
      none ERR-RFP-NOT-FOUND)))

;; Evaluator votes (0..100)
(define-public (vote (id uint) (vendor principal) (score uint))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (asserts! (is-eq (get status rfp) S_EVALUATE) ERR-BAD-TIME)
             (asserts! (<= block-height (get eval-dead rfp)) ERR-BAD-TIME)
             (let ((ev (map-get? evaluators { id: id, who: tx-sender })))
               (match ev
                 somee (let ((e (unwrap! somee somee)))
                         (asserts! (get allowed e) ERR-NOT-EVAL)
                         (match (map-get? votes { id: id, evaluator: tx-sender, vendor: vendor })
                           somev (err u105)
                           none (begin
                                  (map-set votes { id: id, evaluator: tx-sender, vendor: vendor } { score: score })
                                  (match (map-get? tallies { id: id, vendor: vendor })
                                    somet (let ((t (unwrap! somet somet)))
                                            (map-set tallies { id: id, vendor: vendor } { sum: (+ (get sum t) score), count: (+ (get count t) u1) }))
                                    none (map-set tallies { id: id, vendor: vendor } { sum: score, count: u1 }))
                                  (ok true)) ))
                 none (err u104))))
      none ERR-RFP-NOT-FOUND)))

;; Finalize: owner supplies candidate list; winner = highest average
(define-public (finalize (id uint) (candidates (list 50 principal)))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (asserts! (is-eq (get owner rfp) tx-sender) ERR-RFP-NOT-FOUND)
             (asserts! (> block-height (get eval-dead rfp)) ERR-BAD-TIME)
             (let ((best none))
               (let ((res (fold candidates none
                             (lambda (v acc)
                               (let ((t (map-get? tallies { id: id, vendor: v }))
                                     (p (map-get? proposals { id: id, vendor: v })))
                                 (if (and (is-some t) (is-some p))
                                     (let ((rec (unwrap! t t)) (prec (unwrap! p p)))
                                       (let ((avg (div (get sum rec) (get count rec))) (rev (get revealed-at prec)))
                                         (if (is-none acc) (some (tuple v avg rev))
                                             (let ((accv (unwrap! acc acc)))
                                               (if (> avg (get 1 accv)) (some (tuple v avg rev)) acc)))))
                                     acc))))))
                 (match res
                   none (err u109)
                   some-res (let ((winner (get 0 (unwrap! some-res some-res))))
                              (map-set rfps { id: id } (merge rfp { status: S_FINAL, winner: (some winner) }))
                              (let ((bid (+ (var-get next-badge) u1)))
                                (var-set next-badge bid)
                                (map-set badges { bid: bid } { owner: winner, meta: (concat "Award:RFP#" (to-hex (to-buff id))) })
                                (ok (tuple winner bid))))))))
      none ERR-RFP-NOT-FOUND)))
