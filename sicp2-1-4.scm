;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2.1.4 拡張問題：区間算術演算

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;;;; Q 2.7 区間演算の選択子を定義する

(define (make-interval a b) (cons a b))


(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define x (make-interval 10 14))
(upper-bound x)
;> 14
(lower-bound x)
;> 10


;;;; Q 2.8 差の演算
;;; マイナスの区間を加算する
;;; 区間のマイナスは、(ー下限,ー上限)とする
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))
(define x (make-interval 1 2))
(define y (make-interval 4 5))

(add-interval x y)
;> (5 . 7)
(sub-interval x y)
;> (-4 . -2)

;; 検算
(sub-interval (add-interval x y) y)
;> (0 . 3)
;; ??? (1 . 2)にならない？




;;;; Q 2.9 ??
;;; いままで定義した演算について、計算結果の幅と計算前引数（x,yなど）の幅の関係について問うている？
;;; 計算結果の幅だけ欲しいならば、引数の区間の幅だけで計算できる
;;; 和と差の幅は、引数の幅だけで計算可能なことを示せばよいか。


;;;; Q 2.10 0をまたがる区間
;;; make-interval を書き換えたほうがいいような気がする


(define x (make-interval 1 2))
(define y (make-interval -1 3))
(div-interval x y)

(define (div-print y)
  (display (/ 1.0 (upper-bound y)))
  (newline)
  (display (/ 1.0 (lower-bound y))))
(div-print y)
;> 0.3333333333333333
;-1.0#<undef>

;;; 区間の大小が逆転してる

(define (div-interval2 x y)
  (let ((lower (/ 1.0 (upper-bound y)))
        (upper (/ 1.0 (lower-bound y))))
    (if (> lower upper)
        (error "error!")
        (mul-interval x
                      (make-interval lower upper)))))
(define x (make-interval 1 2))
(define y (make-interval -1 3))
(div-interval2 x y)
;> *** ERROR: error!

(define x (make-interval 1 2))
(define y (make-interval 1 3))
(div-interval2 x y)
;> (0.3333333333333333 . 2.0)
(div-interval x y)
;> (0.3333333333333333 . 2.0)


;;; OK



;;;; Q 2.11
;;; 区間の端がマイナス・ゼロ・プラスの場合か。3*3で9パターン
