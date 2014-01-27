;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 準備

(define true #t)
(define false #f)
(define nil '())

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (< n 2) ; 1 除外
      false
      (= n (smallest-divisor n))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SICP 3.5 ストリーム
; http://sicp.iijlab.net/fulltext/x350.html

; 代入にはいろいろ問題があることがわかった。
; モデル化のために代入以外の方法はないか？
;
; 実世界の現象をモデリングする方法
; 時間軸にしたがって変化していく値を代入によって表現した。
; 　実世界で値が変化するタイミングで計算機内の値も変化する、実世界時間と計算機内時間を同一視する考え方。
;
; 変化する値を時間関数で表現できるならば、時間関数の並びとしてモデル化できる。
; 　時間関数、時間を受け取ってその時点での値を返す関数
; これを表現するデータ構造としてストリームを導入する
;
; 並びをリストで表現することも可能だが、性能的にアレなので「遅延評価」を取り入れる
; 遅延評価によって無限個の並びを表現することも可能になる

; ストリーム処理を取り入れることで代入/可変データを使わずに、状態のあるシステムを表現できる
; 　ただし代入、ストリームには一長一短があり、どちらが良いとは言い切れない



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.5.1 ストリームは遅延リスト
; http://sicp.iijlab.net/fulltext/x351.html

; 単純にリストで表現した場合はストリームは性能が悪い

;;; ある区間の素数の総和を計算する（反復法）
(define (sum-primes-itr a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))
(sum-primes-itr 1 10)
;> 17
;; (+ 2 3 5 7) = 17
(sum-primes-itr 1 20)
;> 77
;; (+ 2 3 5 7 11 13 17 19) = 17

;;; accumlate版
(define (sum-primes-ac a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))
(sum-primes-ac 1 10)
;> 17
(sum-primes-ac 1 20)
;> 77


;;; 性能を試す

;(time (sum-primes-itr 10000 1000000))
; real  10.720
; user  10.630
; sys    0.010
;> 37544665627

;; goshの使用メモリ 7.9MBほど

;(time (sum-primes-ac 10000 1000000))
; real  11.281
; user  11.110
; sys    0.060
;> 37544665627

;; goshの使用メモリ 134.2MBほど

;; itr, acの実行時間はそれほど変わらないけど、モニタを見てるとacのほうがメモリを使う

;; acは10000 - 1000000の数値のリストをすべてメモリ上に作る。それを素数判定でフィルタして大部分を捨てていくのでメモリ効率が悪い




;;;; 遅延評価

; 構成子　cons-stream
; 選択子　stream-car, stream-cdr
; 制約
; (stream-car (cons-stream x y)) = x
; (stream-cdr (cons-stream x y)) = y


;;; 第2章のリスト演算のストリーム版を作る
;;; list-ref, map, for-each

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(stream-ref (cons-stream 1 '(2)) 0)
;> 1
(stream-ref (cons-stream 1 '(2)) 1)
;> 2


(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(stream-map (lambda (v) (* v 10))
            (cons-stream 1 '(2)))
;> (10 . #<closure (delay delay)>)
(stream-ref (stream-map (lambda (v) (* v 10))
                        (cons-stream 1 '(2)))
            1)
;> 20


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))


(define (display-stream s)
  (newline)
  (stream-for-each display-line s))


(define (display-line x)
  (display x)
  (newline))

(display-stream (cons-stream 1 (cons-stream 2 the-empty-stream)))
; 1
; 2
; done






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 構成子、選択子の実装

;; ボツ
;(define (cons-stream a b)
;  (cons a (delay b)))

;; 特殊形式
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define s1 (cons-stream 1 (cons-stream 2 '())))
; s1
;> (1 . #<closure (delay delay)>)
(define s2 (cons-stream 1 (cons-stream 2 (cons-stream 3 '()))))
;> (1 . #<closure (delay delay)>)
(define s3 (cons-stream (display 1)
                        (cons-stream (display 2)
                                     (cons-stream (display 3) '()))))
;> 1s3  (display 1)が実行されてる
(stream-ref s3 2)
;> 23#<undef> (display 2) (display 3)が実行されてる

;; define時には評価されない、特殊形式であることが必要


(define (stream-car stream)
  (car stream))

(stream-car s)
;> 1


; SICPの実装
(define (stream-cdr stream)
  (force (cdr stream)))

(stream-cdr s1)
;> (2)
(stream-cdr (stream-cdr s1))
;> ()


;;; 終端判定　脚注54と同じ
(define the-empty-stream '())
(define stream-null? null?)

(stream-null? the-empty-stream)
;> #t
(stream-null? '())
;> #t
(define s (cons-stream 1 (cons-stream 2 '(3))))
(stream-null? s)
;> #f
(stream-null? (stream-cdr s))
;> #f
(stream-null? (stream-cdr (stream-cdr s)))
(stream-null? (stream-cdr (stream-cdr (stream-cdr s))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ストリームの実装の働き

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(cons 10000
      (delay (stream-enumerate-interval 10001 1000000)))
;> (10000 . #<closure (delay delay)>)


(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))



;;; 素数ストリーム
(time
 (stream-car
  (stream-cdr
   (stream-filter prime?
                  (stream-enumerate-interval 10000 1000000)))))
;(time (stream-car (stream-cdr (stream-filter prime? (stream-enumerate-i ...
; real   0.000
; user   0.000
; sys    0.000
;> 10009

;; 一瞬で実行できた



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delayとforceの実装

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; 特殊形式でないのでボツ
;(define (delay exp)
;  (memo-proc (lambda () exp)))

;; 遅延オブジェクトをつくる（特殊形式）
(define-syntax delay
  (syntax-rules ()
    ((delay a)
     (memo-proc (lambda () a)))))


(define a (delay (+ 1 2)))
;> a
; a
;> #<closure (delay delay)>
; (a)
;> 3


;; 遅延オブジェクトを評価
(define (force delayed-object)
  (delayed-object))

(force (delay (+ 1 2)))
;> 3




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ex3.50

;; stream-mapの一般化、複数の引数を取れるようにする
(define (stream-map proc . argstreams) ; 引数もストリームなことに注意
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(display-stream
 (stream-map + (cons-stream 1 (cons-stream 2 '()))
               (cons-stream 10 (cons-stream 20 '()))))
; 11
; 22
; done



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ex3.51
(define (show x)
  (display "> ") (display-line x)
  x)


(define x (stream-map show (stream-enumerate-interval 0 10)))
; 0
;> x

(stream-ref x 5)
; 1
; 2
; 3
; 4
; 5
;> 5

(stream-ref x 7)
; 6
; 7
;> 7



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ex3.52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  (display "sum += ") (display x) (display " : ") (display sum) (newline)
  sum)



(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; 結果 sum = 1
; sum += 1 : 1
; 最初のひとつだけaccumを実行
; accumでsumを返しているので、1..nまでの総和のストリームになる



(define y (stream-filter even? seq))
; 結果 sum = 6
; sum += 2 : 3
; sum += 3 : 6
; delayでメモ化されているので 1 は新たに加算されることはない

; stream-cdrでforceされるので、その回数を数えれば良い
; (define (stream-filter pred stream)
;   (cond ((stream-null? stream) the-empty-stream)
;         ((pred (stream-car stream))
;          (cons-stream (stream-car stream)
;               (stream-filter pred (stream-cdr stream))))  ; 特殊形式だからここは無視
;         (else (stream-filter pred (stream-cdr stream))))) ; ここがどれだけ実行されるか
;
; 総和ストリーム 1 3 6 10 ...
; 偶数フィルタなので6まで評価される。それ以降はcons-streamで包まれる
; 総和sumは6まで実行される



(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; sum = 10
; remainder = mod:  5の倍数のフィルタ
; 上記と同じような議論で
; 総和ストリームの5の倍数10まで評価される
; sum=10



(stream-ref y 7)
; 結果 sum = 136

; sum += 6 : 21
; sum += 7 : 28  < even
; sum += 8 : 36  < even
; sum += 9 : 45
; sum += 10 : 55
; sum += 11 : 66  < even
; sum += 12 : 78  < even
; sum += 13 : 91
; sum += 14 : 105
; sum += 15 : 120  < even
; sum += 16 : 136  < even
;> 136

;; 総和ストリームの7つ目の偶数
; 10はすでにメモ化されているので印字されない




(display-stream z)
; 結果sum = 210

; 10
; 15
; 45
; 55
; 105
; 120
; sum += 17 : 153
; sum += 18 : 171
; sum += 19 : 190
; 190
; sum += 20 : 210
; 210
; done

;; yのstream-refでメモ化されているものは印字されない
;; (stream-ref y), (display-stream z)の評価順番を変えても値にかわりなし




; メモ化なしのdelay
;(define-syntax delay
;  (syntax-rules ()
;    ((delay a)
;     (lambda() a))))
;; いったんインタプリタを落として、上のdelayをコメントアウトして評価しなおしたほうが良いらしい

;--
(define sum 0)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; 結果  sum = 1
; sum += 1 : 1

(define y (stream-filter even? seq))
; 結果 sum = 6
; sum += 2 : 3
; sum += 3 : 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; 結果  sum = 15
; sum += 2 : 8
; sum += 3 : 11
; sum += 4 : 15


(stream-ref y 7)
; 結果  sum = 162
; sum += 4 : 19
; sum += 5 : 24
; sum += 6 : 30
; sum += 7 : 37
; sum += 8 : 45
; sum += 9 : 54
; sum += 10 : 64
; sum += 11 : 75
; sum += 12 : 87
; sum += 13 : 100
; sum += 14 : 114
; sum += 15 : 129
; sum += 16 : 145
; sum += 17 : 162


(display-stream z)
; 結果  sum = 362
; 15
; sum += 5 : 167
; sum += 6 : 173
; sum += 7 : 180
; 180
; sum += 8 : 188
; sum += 9 : 197
; sum += 10 : 207
; sum += 11 : 218
; sum += 12 : 230
; 230
; sum += 13 : 243
; sum += 14 : 257
; sum += 15 : 272
; sum += 16 : 288
; sum += 17 : 305
; 305
; sum += 18 : 323
; sum += 19 : 342
; sum += 20 : 362
; done


;;; 結果は同じだが、メモ化されていないので、評価ごとにsumへの加算が行われる。
;;; 評価の回数や順番に依存するようになるので、代入のときの短所が解消されない









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SICP3.5.2 無限ストリーム

; ストリームの必要な分しか評価しないというトリックを使うと無限に長い並びを表現できる


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(stream-ref (integers-starting-from 0)
            3)
;> 3
(stream-ref (integers-starting-from 10)
            3)
;> 13


;;; 自然数の無限ストリーム
(define integers (integers-starting-from 1))

(stream-ref integers 300)


;;; 7で割り切れない整数のストリームを定義する

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(stream-ref no-sevens 0)
;> 1
(stream-ref no-sevens 5)
;> 6
(stream-ref no-sevens 6)
;> 8


(stream-ref no-sevens 100)
;> 117



;;; 同じようにフィボナッチ数の無限ストリームを作る
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))
(stream-ref fibs 0) ;> 0
(stream-ref fibs 1) ;> 1
(stream-ref fibs 2) ;> 1
(stream-ref fibs 3) ;> 2
(stream-ref fibs 4) ;> 3
(stream-ref fibs 5) ;> 5
(stream-ref fibs 6) ;> 8
(stream-ref fibs 7) ;> 13




;;;; エラトステネスのふるいを使って素数の無限ストリームを作る（衝撃的だー！）

;; 2の倍数以外でフィルタし、そのcarでまたフィルタ、を繰り返す

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 0) ;> 2
(stream-ref primes 1) ;> 3
(stream-ref primes 2) ;> 5
(stream-ref primes 3) ;> 7
(stream-ref primes 4) ;> 11

(stream-ref primes 50)
;> 233


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ストリームの暗黙の定義

;; (1 1 1 ...)の無限ストリーム
(define ones (cons-stream 1 ones))
(stream-ref ones 0) ;> 1
(stream-ref ones 100) ;>1


(define (add-streams s1 s2)
  (stream-map + s1 s2))

;(display-stream (add-streams '(1 2) '(10 20)))

;; integersを次のように定義出来る
(define integers (cons-stream 1 (add-streams ones integers)))
;; 1
;; 1 + 1 : 2
;; 1 + 2 : 3
;; 1 + 3 : 4
;; ...
;;; ちょっと頭がこんがらがってくるけど、定義から考えれば大丈夫


;; このスタイルでフィボナッチ数も定義できる
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
(stream-ref fibs 0) ;> 0
(stream-ref fibs 1) ;> 1
(stream-ref fibs 2) ;> 1
(stream-ref fibs 3) ;> 2
(stream-ref fibs 4) ;> 3
(stream-ref fibs 5) ;> 5
(stream-ref fibs 6) ;> 8
(stream-ref fibs 7) ;> 13



;; ストリームに定数を掛ける
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(stream-ref (scale-stream integers 7) 6)
;> 49  ; 7の倍数7個目


(define double (cons-stream 1 (scale-stream double 2)))
(stream-ref double 0) ;> 1
(stream-ref double 1) ;> 2
(stream-ref double 2) ;> 4
(stream-ref double 3) ;> 8


;;; 素数ストリームの別定義
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes)) ; primesストリームを使っている

(stream-ref primes 0) ;> 2
(stream-ref primes 1) ;> 3
(stream-ref primes 2) ;> 5
(stream-ref primes 3) ;> 7
(stream-ref primes 4) ;> 11
(stream-ref primes 5) ;> 13
(stream-ref primes 6) ;> 17
(stream-ref primes 7) ;> 19


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ex3.53

(define s (cons-stream 1 (add-streams s s)))

; s1:   1 2 4 ... n
; s2:   1 2 4 ... n
;-------------------------------
; s:  1 2 4 8 ... 2^n

; (1 2 4 8 ...
; 2のべき乗のストリーム

; 確認
(stream-ref s 0) ;> 1
(stream-ref s 1) ;> 2
(stream-ref s 2) ;> 4
(stream-ref s 3) ;> 8
(stream-ref s 4) ;> 16
(stream-ref s 5) ;> 32
(stream-ref s 6) ;> 64
(stream-ref s 7) ;> 128



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ex3.54

; add-streamsに似て, 二つのストリームの要素毎の積を生じる手続き mul-streamsを定義せよ.

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define mul-s (mul-streams (cons-stream 2 (cons-stream 3 the-empty-stream))
                           (cons-stream 5 (cons-stream 7 the-empty-stream))))
(display-stream mul-s)
; 10
; 21
; done


; これとintegersストリームを使い, (0から数えて)n番目の要素がn + 1の階乗になる次のストリームの定義を完成せよ


(define factorials
  (cons-stream 1 (mul-streams integers
                              factorials)))
; (stream-cdr integers)  : 2   3   4   5   6 ...
; factorials             : 1   2   6  24 120 ...
;------------------------------------------------
; mul-streams            : 2   6  24 120 720 ...

(stream-ref factorials 0) ;> 1
(stream-ref factorials 1) ;> 2
(stream-ref factorials 2) ;> 6
(stream-ref factorials 3) ;> 24
(stream-ref factorials 4) ;> 120
(stream-ref factorials 5) ;> 720



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ex3.55

;引数としてストリームSをとり, その要素がS0, S0 + S1, S0 + S1 + S2, ... であるようなストリームを返す手続き partial-sumsを定義せよ. 例えば(partial-sums integers) はストリーム1, 3, 6, 10, 15, ... を返すものとする.

(define (partial-sums s)
  (define sum (cons-stream (stream-car s)   ;s0
                           (add-streams (stream-cdr s)
                                        sum)))
  sum)

(stream-ref (partial-sums integers) 0) ;> 1  : 1
(stream-ref (partial-sums integers) 1) ;> 3  : 1 + 2
(stream-ref (partial-sums integers) 2) ;> 6  : 1 + 2 + 3
(stream-ref (partial-sums integers) 3) ;> 10 : 1 + 2 + 3 + 4
(stream-ref (partial-sums integers) 4) ;> 15 : 1 + 2 + 3 + 4 + 5







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 便利手続きを作った
;;; stream-take, stream-list

;;; 遅延ストリームの先頭n個を取る
(define (stream-take s n)
  (if (< n 1)
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s)
                         (- n 1)))))

(stream-take integers 6)
;> (1 2 3 4 5 6)
(stream-take (partial-sums integers) 5)
;> (1 3 6 10 15)



;;; listのストリーム版
(define (stream-list . items)
  (if (null? items)
      the-empty-stream
      (cons-stream (car items)
                   (apply stream-list (cdr items)))))
(stream-list 1 2 3 4)
;> (1 . #<closure (memo-proc memo-proc)>)

(display-stream (stream-list 1 2))
; 1
; 2
; done

(stream-take (stream-list 1 2 3 4) 2)
;> (1 2)


