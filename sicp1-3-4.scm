;;;; 前節で定義した手続き
(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
;    (display guess) (newline) ; debug
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1.3.4 値として返される手続き

;;; 平均緩和法に変換した手続きを返す
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)
;> 55

;; x^2 -> (x + x^2)/2
;; (10 + 10^2)/2 = 110/2 = 55.
;; 上記の55は合っている。


;;; average-dampを使ってsqrtを定義する
(define (sqrt-ad x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(sqrt-ad 2)
;> 1.4142135623746899

;;; average-dampをつかって三乗根を求める
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
(cube-root 27)
;> 3.000022143521597




;;;; Newton法

;;; 微分の limの下で極限0のやつを以下で近似
(define dx 0.00001)

;;; 1変数関数の手続きを 微分した手続きを 返す手続き
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;;; f(x) = x^3
(define (cube x) (* x x x))

;; Df(x) = 3x^2
;; Df(5) = 3*5^2 = 75
((deriv cube) 5)
;> 75.00014999664018
;; OK!




;;;; Newton法を不動点プロセスとして表す

;;; 1変数手続きを受け取って、Newton法用の手続きに変換して返す
;;; 最初の例でいえば g を受け取って f にして返す
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

;;; Newton法で手続きの零点を探す
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; テスト：平方根を計算する
;;; y |-> y^2 - x の零点を見つける

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(sqrt 2)
;> 1.4142135623822438



; TODO 不動点見つける　が　零点を見つける　になる理由説明が必要。
; TODO 平均緩和法とNewton法のステップ数を比較したい






;;;; 抽象と第一級手続き

;; 平均緩和法もNewton法も「手続きを変換して不動点を見つける」というパターンが同じ。
;; 処理を共通化できそう

;;; 手続きとそれを変換する手続きを受け取って、不動点を探す
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;; 平均緩和法で平方根を出す手続きを上記で書きなおす
(define (sqrt-fpot-ad x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(sqrt-fpot-ad 2)
;> 1.4142135623746899


;;; Newton法で平方根を出す手続きを上記で書き直す
;; 渡す手続きが違うので注意！
(define (sqrt-fpot-nt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(sqrt-fpot-nt 2)
;> 1.4142135623822438

;; 精度比較
;ad> 1.4142135623 746899
;nt> 1.4142135623 822438
;    1.4142135623 730950488016887242096...

; TODO なんかNewton法のほうが精度悪いんだけど。。。



;; 抽象化についてのところを3行でまとめる
;; ・プログラムを書く上で抽象化を心がけることは大事
;; ・でも抽象化しすぎも良くない、ほどよい抽象化ができるように抽象化の方法は覚えておくべき
;; ・高階手続きは、手続きをプログラム言語の要素として表せる抽象化だから便利で重要


;;;; Lispは手続きに第一級の身分を与えた

;; 第一級要素の「権利と特権」

;; ・変数として名前が付けられる
;;    -> define で手続きに名前が付けられる

;; ・手続きに引数として渡せる
;;    -> 渡せる。1.3.1〜3節で確認済み。

;; ・手続きの結果として返される
;;     ->返せる。1.3.4節（今回）で確認済み。

;; ・データ構造に組み込める
;;     ->COMING SOON...
;;       脚注65によると　2章でデータ構造を紹介した後に分かるらしい

;; これらができるとプログラムでプログラムをいろいろ操作できるから大事っぽい





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Q 1.40

;;; 三次式 y = x^3 + ax^2 + bx + c
;;; を計算する手続きを返す
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;; TEST: y = x^3
((cubic 0 0 0) 3)
;> 27
;; TEST: y = x^3 + 1
((cubic 0 0 1) 3)
;> 28
;; TEST: y = x^3 + 2x + 1
((cubic 0 2 1) 3)
;> 34
;; TEST: y = x^3 + 4x^2 + 2x + 1
((cubic 4 2 1) 3)
;> 70


;;; 上記の三次式の零点を計算してみる
(newtons-method (cubic 4 2 1) 1)
;> -3.5115471416944994
;; 検算
((cubic 4 2 1) -3.5115471416944994)
;> 3.561595462997502e-13
;; ほぼ 0
;; OK




;;;; Q 1.41

;;; 1引数手続きを2回作用させる手続きを返す手続き
(define (double f)
  (lambda (x) (f (f x))))

;;; 引数に＋１する手続き
(define (inc x) (+ x 1))
(inc 2)
;> 3

;; TEST 引数に＋２する手続きを返す
((double inc) 5)
;> 7


;; Q.以下の手続きは何を返すか
(((double (double double)) inc) 5)

;; 説明のため、n回作用させる関数をapply-Nと書いて表現する
;(((apply-2 (apply-2 apply-2)) inc) 5)
;(((apply-2 apply-4) inc) 5)
;; (apply-4 (apply-4 ...
;; 4回作用させるのを4回作用させてる
;; 回数は4^2
;((apply-16 inc) 5)
;; incを16回作用させてる
;; 答えは5+16=21のはず

(((double (double double)) inc) 5)
;> 21
;; OK!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Q 1.42

;;; 手続きを合成する手続き
(define (compose f g)
  (lambda (x) (f (g x))))

;; TEST
((compose square inc) 6)
;> 49
((compose inc inc) 3)
;> 5


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Q 1.43

;;; f をn回作用させる手続き
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated inc 2) 5)
;> 7
((repeated square 2) 2)
;> 16



;;; 最初に書いたバージョン
;; 手続きを値として扱うことに慣れてなかったせい？
(define (repeated-first f n)
  (lambda (x)
    (define (rec f x2 n)
      (if (= n 0)
          x2
          (f (rec f x2 (- n 1)))))
    (rec f x n)))

((repeated-first inc 2) 5)
;> 7
((repeated-first square 2) 2)
;> 16


;;; おまけ
;;; repeatedを反復的プロセスで書いてみた
(define (repeated-iter f n)
  (define (iter f n ret)
    (if (= n 1)
        ret
        (iter f (- n 1) (compose f ret))))
    (iter f n f))

((repeated-iter inc 2) 5)
;> 7
((repeated-iter square 2) 2)
;> 16





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Q 1.44
;; 関数の平滑化

;;; 手続きを受け取って平滑化した手続きを返す
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) ; dx は微分derivを定義するときにつかったものを流用
          (f x)
          (f (+ x dx)))
       3.0)))

(square 2)
;> 4
((smooth square) 2)
;> 4.000000000066667

(sin 1.0)
;> 0.841470984 8078965
((smooth sin) 1.0)
;> 0.841470984 7798475


; TODO 平滑化はなにをやっているのか？
; 　　傾きが少なくなっていく？
;     x軸に対して平行に近づいていく？


(define (n-fold-smoothed f n)
  ((repeated smooth n) f)) ; (smooth (smooth .. f) .. )

(square 2)
;> 4
((n-fold-smoothed square 1) 2)
;> 4.000000000066667
((n-fold-smoothed square 2) 2)
;> 4.000000000133333
((n-fold-smoothed square 10) 2)
;> 4.000000000666667
;((n-fold-smoothed square 20) 2)
;; 返ってこないぞ。。。

(sin 1.0)
;> 0.841470984 8078965
((n-fold-smoothed sin 1) 1.0)
;> 0.841470984 7798475

((n-fold-smoothed sin 15) 1.0)
;; 単純に処理が糞重いだけか？
(time ((n-fold-smoothed sin 15) 1.0))
;(time ((n-fold-smoothed sin 15) 1.0))
; real   2.677
; user   2.660
; sys    0.000


;; いろいろ試した

;(time ((n-fold-smoothed square 15) 2))
; real   2.120
; user   2.110
; sys    0.000
4.000000001

;(time ((n-fold-smoothed square 16) 2))
; real   6.339
; user   6.300
; sys    0.020

;(time ((n-fold-smoothed square 20) 2))
; real 530.126
; user 523.380
; sys    1.420
4.000000001333333


;; 糞重くなる説明
;; 平滑化1回でf を3回計算している
;; それをさらに平滑化するとｆを9回計算することになる
;; ｎ重平滑化はfを3^n回計算する

;; 3^nに手続きが増える
;; 3^10 =      59049
;; 3^15 =   14348907
;; 3^20 = 3486784401
;; n-fold-smoothedのオーダーは、O(3^n)

;; 実行時間で検証
;; n=15, time=2.120
;; n=16, time=6.339
;;  約3倍増えている
;; n=20 time?
(* 6.339 (* 3 3 3 3))
;> 513.4590000000001
; real 530.126
;; 大体あってる


;; TODO ｎ重平滑化関数をもっと効率化できないか。




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Q 1.45

;; TODO なぜ4乗根の計算に単一の平均緩和法が使えないのか？


;;; 安全に計算の動きをおうため、計算回数の上限を指定できるように改造した
(define (fixed-point-limited f first-guess limit)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (let ((next (f guess)))
      ;; 計算過程を表示する
      (display count) (display ": ") (display next) (newline)
      (if (or (= count limit) (close-enough? guess next))
          (list count next) ; 計算回数も一緒に返す
          (try next (+ count 1)))))
  (try first-guess 1))

;;; 平均緩和法1回での4乗根計算手続き
(define (root4-ad1 x limit)
  (fixed-point-limited
    (average-damp (lambda (y) (/ x
                                 (* y y y))))
   1.0
   limit))

(root4-ad1 16 10)
;> (10 1.8597959434977196)

(root4-ad1 16 100)
;> (100 1.9364452032204889)
(root4-ad1 16 1000)
;> (1000 1.9781656271972445)

;(time (root4-ad1 81 0))
; real 176.408
; user 175.530
; sys    0.380
;> (449999982 3.000050001249936)


;; 4乗根を求める場合、「単一の平均緩和法は（中略）収束するのに十分ではない」
;; とあるけど、収束自体はする。　ただし超遅くなる

;; 不動点を求めるときの関数グラフはできるだけx軸に平行なほうがよい
;; 傾きが0に近く、緩やかに増加していくグラフだとよいはず

;; 不動点よりしたにグラフが下がってるとマズイのか？

;; 不動点より正の方向の関数の値は、増加して行かないとマズイ。
;; （斜め45度の線より下の値の話し）
;; そうじゃないと不動点をもとめる値が不動点を中心として螺旋状になってしまう。
;; 不動点の両端から狭めていくから、単純に考えると計算が2倍になる

;; N回平均緩和法したのを微分して最下点を〜とか考えたけどギブアップ！
;; 地道にグラフ書いた結果から発見的に答え出すかと思ったがそれも面倒くさい


;;;; 問題文にあるようにいろいろ実験してみる


;;; 不動点探索が螺旋状になってないかチェックする
;;; 増加ｏｒ減少が続いたらOK
;;; 増加と減少が交互にきたらNG
(define (fixed-point-check f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ;; 値の増減を返す
  (define (value-sign v1 v2)
    (cond ((< v1 v2) 1)
          ((> v1 v2) -1)
          (else 0)))
  (define (try guess sign)
    (let ((next (f guess)))
      ;; letで束縛した変数をlet内で使えないらしい
      (let ((next-sign (value-sign guess next)))
        (cond ((= next-sign 0) next) ; すでに不動点
              ((= sign next-sign) ; 符号が同じなら単調増加or減少
               (display "ok!") (newline)
               (display sign)  (newline)
               (display guess) (newline)
               (display next))
              ((not (= sign next-sign)) ; 符号が違う＝増加減少が交互
               (display "NG")  (newline)
               (display sign)  (newline)
               (display next-sign))
              (else
               (if (close-enough? guess next)
                   next
                   (try next next-sign)))))))
  ;; 初期値によって増加減少が変わるので、何度か計算してから探索を始める
  (let ((guess1 (f (f first-guess)))
        (guess2 (f (f (f first-guess)))))
    (try guess2 (value-sign guess1 guess2))))

;; 4乗根の計算で実験する
;; 平均緩和法1回の場合
(fixed-point-check
 (average-damp
  (lambda (y) (/ 2.0 (* y y y))))
 1.0)
;; 平均緩和法2回の場合
(fixed-point-check
 (average-damp
  (average-damp
   (lambda (y) (/ 2.0 (* y y y)))))
 1.0)


;;; 平均緩和法をｍ回適用してｎ乗根を見つける手続き
;;; 平均緩和法の適用回数を特定するための実験版
(define (root-n-test x n m)
  (fixed-point-check
   ((repeated average-damp m)
    (lambda (y)
      (/ x
         (expt y (- n 1))))) ; exptはべき乗関数
   1.0))

;; 4乗根　平均緩和法1回適用
(root-n-test 8.0 4 1)
;> NG
;-1
;1#<undef>

;; 4乗根　平均緩和法2回適用
(root-n-test 8.0 4 2)
;> ok!
;-1
;1.8178268004543974
;1.6963150945303935#<undef>


;; 5乗根のテスト　3回適用でOK
(root-n-test 2.0 5 1) ; NG
(root-n-test 2.0 5 2) ; NG
(root-n-test 2.0 5 3) ; OK!


;; 6乗根のテスト
(root-n-test 2.0 6 1) ; NG
(root-n-test 2.0 6 2) ; NG
(root-n-test 2.0 6 3) ; OK!

;; 7乗根のテスト
(root-n-test 2.0 7 1) ; NG
(root-n-test 2.0 7 2) ; NG
(root-n-test 2.0 7 3) ; OK!

;; 8乗根のテスト
(root-n-test 2.0 8 1) ; NG
(root-n-test 2.0 8 2) ; NG
(root-n-test 2.0 8 3) ; OK!

;; 20乗根のテスト
(root-n-test 2.0 20 1) ; NG
(root-n-test 2.0 20 2) ; NG
(root-n-test 2.0 20 3) ; NG
(root-n-test 2.0 20 4) ; NG
(root-n-test 2.0 20 5) ; OK!

;; もしかしてｎ乗根の平均緩和法適用回数はｎの平方根なんでは？
;; ー＞ダメでした

;; 100乗根
(root-n-test 2.0 100 6) ; NG
(root-n-test 2.0 100 7) ; ok!
(root-n-test 2.0 100 8) ; ok!
(root-n-test 2.0 100 9) ; ok!

;; 1000乗根
(root-n-test 2.0 1000  6) ; NG
(root-n-test 2.0 1000  7) ; NG
(root-n-test 2.0 1000  8) ; NG
(root-n-test 2.0 1000  9) ; NG
(root-n-test 2.0 1000 10) ; ok!


;; カンニングした結果と実験結果をもとにすると1000乗根の平均緩和法の回数はこうなる
(ceiling (/ (log 1000) (log 2)))
;> 10.0

;; なぜこうなるか、その説明は./1-3-4/root-n-calc.pdfにまとめておいた



;;; ｎ乗根を探す手続き
(define (root-n x n)
  (fixed-point
   ((repeated average-damp
                ;; 回数：2を底とするlog nの小数点以下切上げ
                (ceiling (/ (log n) (log 2))))
    (lambda (y) (/ x
                   (expt y (- n 1))))) ; exptはべき乗
   1.0))

;; test
(root-n 2 2)
;> 1.4142135623746899
(root-n 27 3)
;> 3.0000234260125787
(root-n 256 8)
;> 2.0000000000039666
(root-n 65536 16)
;> 2.000000000076957



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Q 1.46
;;;; 反復改良法を一般化する

;;; 予測値の良好さチェック手続きと、予測値改良手続きを受け取り、予測値を改良する手続きを返す
(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
            (iter next))))
    (iter first-guess)))

;;; 1.1.7節のsqrtをiterative-improveをつかって書き直す
(define (sqrt-ii x)
  ((iterative-improve
    (lambda (v1 v2) (< (abs (- v1 v2)) tolerance)) ; fixed-pointで使ってるやつ
    (lambda (v)
      (average v (/ x v))))
   1.0))

(sqrt-ii 2)
;> 1.4142135623746899
(sqrt-ii 3)
;> 1.7320508100147274
(sqrt-ii 25)
;> 5.000000000053722
(sqrt-ii 49)
;> 7.000000000000002
(sqrt-ii 144)
;> 12.0


;;; 1.3.3節のfixed-pointをiterative-improveをつかって書きなおす
(define (fixed-point-ii f first-guess)
  ((iterative-improve
    (lambda (v1 v2)
      (< (abs (- v1 v2)) tolerance))
    f)
   first-guess))


;; TEST cos
(fixed-point cos 1.0)
;> 0.7390547907469174
(fixed-point-ii cos 1.0)
;> 0.7390547907469174
;; OK



;;;; 第1章 終了！
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

