;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  5. レジスタ計算機


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 5.1 レジスタ計算機の設計

;;; ex5.1 階乗計算機のデータパス図、制御図




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 5.1.1 レジスタ計算機の記述言語


;; GCD計算機 データパス図記述言語
(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))

 (operations
  ((name rem)
   (inputs (register a) (register b)))
  ((name =)
   (inputs (register b) (constant 0)))))

;; GCD計算機 制御図(controller)記述言語
(controller
 test-b                           ; ラベル
   (test =)                       ; テスト
   (branch (label gcd-done))      ; 条件分岐
   (t<-r)                         ; ボタン押す
   (a<-b)                         ; ボタン押す
   (b<-t)                         ; ボタン押す
   (goto (label test-b))          ; 無条件分岐
 gcd-done)                        ; ラベル



;; 改良版GCD計算機　記述言語
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)





;;; ex5.2 レジスタ計算機言語を使い, 問題5.1の反復的な階乗計算機を記述せよ

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;;; 階乗計算機 レジスタpが階乗の答え
(controller
   (assign p (const 1))
   (assign c (const 1))
 test-c
   (test (op >) (reg c) (reg n))
   (branch (label factorial-done))
   (assign p (op *) (reg p) (reg c))
   (assign c (op +) (reg c) (const 1))
   (goto (label test-c))
 factorial-done)


;;;; 働き
;; read,inputを導入
;; 計算機に関係ない働き（action）を起こすためperformを導入
; 例 レジスタaを印字
; (perform (op print) (reg a))


;;; 図5.4 入力を読み込み結果を印字するGCD計算機
(controller
  gcd-loop
    (assign a (op read))
    (assign b (op read))
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
  gcd-done
    (perform (op print) (reg a))
    (goto (label gcd-loop)))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 5.1.2 計算機設計における抽象

;;; 剰余のような特別な命令を基本的な命令に変換する
(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))


;;; GCD計算機のremを上記に置き換える
;;; 図5.6 図5.5のGCD計算機の制御器の命令列
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (reg a))
 rem-loop
   (test (op <) (reg t) (reg b))
   (branch (label rem-done))
   (assign t (op -) (reg t) (reg b))
   (goto (label rem-loop))
 rem-done
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)


;;; ex5.3 Newton法計算機　特別命令あり版とそれの展開版

;;; good-enough?, improve命令がある場合
;;; データパス図、制御図
;;; 記述言語
(controller
   (assign g (const 1.0))
 test-good-enough?
   (test (op good-enough?) (reg g))
   (branch (label newton-done))
   (assign g (op improve) (reg g))
   (goto (label test-good-enough?))
 newton-done)

;;; good-enough?, improveのデータパス図

;;; 展開した記述言語
(controller
   (assign g (const 1.0))
 test-good-enough?
   ;; good-enough? の展開
   (assign t1 (op *) (reg g) (reg g))
   (assign t2 (op -) (reg t1) (reg x))
   (test (op >=) (reg t2) (const 0))      ; abs
   (branch (label skip-abs))
   (assign t2 (op *) (reg t2) (const -1)) ; 負数は正数に変換
 skip-abs
   (test (op <) (reg t2) (const 0.001))
   ;; good-enouh? 完了
   (branch (label newton-done))
   ;; improveの展開
   (assign t1 (op /) (reg g) (reg x))
   (assign t2 (op +) (reg g) (reg t1))
   (assign t3 (op /) (reg t2) (const 2))  ; average
   ;; improve 完了
   (assign g (reg t3))
   (goto (label test-good-enough?))
 newton-done)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 5.1.3 サブルーチン






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 5.1.4 再帰を実装するためのスタックの使用

;;; ex5.4
;; 次の手続きのそれぞれを実装するレジスタ計算機を規定せよ. 各計算機に対し, 制御器の命令列を書き, データパスを示す図を描け

;; a. 再帰的べき乗:
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; 記述言語
(controller
   (assign val (const 1))
   (assign continue (label expt-done))
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (reg continue))
   (save continue)
   (assign continue (label after-expt))
   (assign n (op -) (reg n) (const 1))
   (goto (label expt-loop))
 after-expt
   (restore continue)
   (assign val (op *) (reg val) (reg b))
   (goto (reg continue))
 expt-done)    ; valが答え



;; b. 反復的べき乗:
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

;; 記述言語
(controller
   (assign p (const 1))
   (assign c (reg n))
 test-c
   (test (op =) (reg c) (const 0))
   (branch (label expt-done))
   (assign p (op *) (reg p) (reg b))
   (assign c (op -) (reg c) (const 1))
   (goto (label test-c))
 expt-done)   ; pが答え


;;; ex5.5
;; あまり馬鹿げていない(少なくとも一回の再帰呼出しの実行が必要な)値を使い, 階乗とFibonacci計算機を机上シミュレートせよ. 実行の主要な場所でのスタックの内容を示せ
;; (fact 3)
;; (fib 2)



;;; ex5.6
;; Ben BitdiddleはFibonacci計算機の制御列にはそれを除去すると速くなり得る余分なsaveと余分なrestoreがあると見た. その命令はどれか


;; 図5.12 Fibonacci数を計算する計算機の制御器
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; Fib(n-1)を計算するよう設定
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; nの昔の値を退避
   (assign n (op -) (reg n) (const 1)); nを n-1 に変える
   (goto (label fib-loop))            ; 再帰呼出しを実行
 afterfib-n-1                         ; 戻った時 Fib(n-1)はvalにある
   (restore n)
   (restore continue)  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; restore
   ;; Fib(n-2)を計算するよう設定
   (assign n (op -) (reg n) (const 2))
   (save continue) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; restore->save のあいだでcontinueは変わってないので無駄処理
   (assign continue (label afterfib-n-2))
   (save val)                         ; Fib(n-1)を退避
   (goto (label fib-loop))
 afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
   (assign n (reg val))               ; nにはFib(n-2)がある
   (restore val)                      ; valにはFib(n-1)がある
   (restore continue)
   (assign val                        ; Fib(n-1)+Fib(n-2)
           (op +) (reg val) (reg n))
   (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
 immediate-answer
   (assign val (reg n))               ; 基底の場合: Fib(n)=n
   (goto (reg continue))
 fib-done)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 5.1.5 命令の要約

;;次の命令は5.1.1節で説明した:
;(assign ⟨register-name⟩ (reg ⟨register-name⟩))
;(assign ⟨register-name⟩ (const ⟨constant-value⟩))
;(assign ⟨register-name⟩ (op ⟨operation-name⟩) ⟨input1⟩ ... ⟨inputn⟩)
;(perform (op ⟨operation-name⟩) ⟨input1⟩ ... ⟨inputn⟩)
;(test (op ⟨operation-name⟩) ⟨input1⟩ ... ⟨inputn⟩)
;(branch (label ⟨label-name⟩))
;(goto (label ⟨label-name⟩))

;; ラベルを保持するレジスタの使用は5.1.3節で説明した:
;(assign ⟨register-name⟩ (label ⟨label-name⟩))
;(goto (reg ⟨register-name⟩))

;;スタックを使う命令は5.1.4節で説明した:
;(save ⟨register-name⟩)
;(restore ⟨register-name⟩)
