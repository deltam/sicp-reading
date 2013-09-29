;; 真偽値alias
(define true #t)
(define false #f)


;;;; SICP 2.3.4 例 Huffman符号化木

;; 可変長符号
;; 　可変長ならば、多く出現する記号に短い符号を割り当てて、データ量を削減できる


;; 記号ごとに符号の長さが違う場合、いま読んでる符号の長さをどう知るか？
;; １　符号の間に予め決めておいた分離符号をおく（モールス信号の方式）
;; ２　ぜんぶの符号が違う語頭を持つように調整する（Huffmanの方法）

;; では語頭がちがう効率のよい可変長符号をどうすれば生成できるか？
;;  記号「アルファベット」の相対頻度が与えられているとする

;; 出現頻度


;;;; Huffman木の表現


;;; Huffman木の葉の構成子
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(make-leaf 'a 10)
;> (leaf a 10)

(define (leaf? object)
  (eq? (car object) 'leaf))
(leaf? (make-leaf 'a 10))
;> #t
(leaf? '(hoge a 10))


(define (symbol-leaf x) (cadr x))
(symbol-leaf (make-leaf 'a 10))
;> a

(define (weight-leaf x) (caddr x))
(weight-leaf (make-leaf 'a 10))
;> 10


;;; 葉or木を受け取って新たな木を返す
;;;  左右の木に属している記号のリスト、頻度の合計を持つ
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define hoge-tree
  (make-code-tree (make-leaf 'a 1) (make-leaf 'b 2)))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

;;; 木に属している記号のリストを返す
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

;;; 木に属している記号の頻度合計
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define code-tree (make-code-tree
                   (make-leaf 'a 10)
                   (make-leaf 'b 9)))
;code-tree
;> ((leaf a 10) (leaf b 9) (a b) 19)

(left-branch code-tree)
;> (leaf a 10)

(right-branch code-tree)
;> (leaf b 9)

(symbols code-tree)
;> (a b)

(weight code-tree)
;> 19





;;;; 復号化手続き

;;; 復号
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;;; 現在のBitに基いて左右の枝を選ぶ
;;; 0＝左、1=右
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define hoge-tree
  (make-code-tree
   (make-leaf 'a 10)
   (make-code-tree
    (make-leaf 'b 2)
    (make-leaf 'c 2))))

(decode '(0 1) hoge-tree)
;> (a b)
(decode '(1 0) hoge-tree)
;> (b a)
(decode '(1 1 0 0) hoge-tree)
;> (b b a a)
(decode '(1 1 0 0 3) hoge-tree)
;> *** ERROR: bad bit -- CHOOSE-BRANCH 3



;;;; 葉の集合を操作する関数を作る

;;; 順序付けられている葉の集合に新たな葉を付け足す
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(adjoin-set (make-leaf 'D 3) '((leaf A 1) (leaf C 2) (leaf B 10)))
;> ((leaf A 1) (leaf C 2) (leaf D 3) (leaf B 10))
;; 出現頻度が少ない順に順序付けられている


;;; 記号、頻度のペア集合を葉の集合に変換する
;; pairsは順序付けられて無くてもOK
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; 記号
                               (cadr pair))  ; 頻度
                    (make-leaf-set (cdr pairs))))))

(make-leaf-set '((a 10) (b 3)))
;> ((leaf b 3) (leaf a 10))

(make-leaf-set '((a 10) (b 3) (c 1)))
;> ((leaf c 1) (leaf b 3) (leaf a 10))
;; 頻度少ない順にソートされる





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ex2.67　サンプルの復号

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(print sample-tree)
;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
;#<undef>

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;> (A D A B B C A)

;; 木の形
;; ｰ>  ./2-3-4/sample-tree.gif



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ex2.68 メッセージの符号化

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; 記号をビット列に変換する
(define (encode-symbol x tree)
  (cond ((leaf? tree) '())
        ((element-of-set? x (symbols (left-branch tree)))
;        ((memq x (symbols (left-branch tree))) ; 記号限定だからmemqでもいい気がする
         (cons 0 (encode-symbol x (left-branch tree))))
        ((element-of-set? x (symbols (right-branch tree)))
;        ((memq x (symbols (right-branch tree)))
         (cons 1 (encode-symbol x (right-branch tree))))
        (else (error "Nothing Symbol" x))))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(encode-symbol 'A sample-tree)
;> (0)
(encode-symbol 'B sample-tree)
;> (1 0)
(encode-symbol 'D sample-tree)
;> (1 1 0)

(encode '(A A B D) sample-tree)
;> (0 0 1 0 1 1 0)

;; ./2-3-4/sample-tree.gif と見比べながら確認




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ex2.69 generate-huffman-tree

;;; Huffman木を作成する
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;;; 頻度が最少の2つを結合するのを繰り返す。要素がひとつになったら終了
(define (successive-merge set)
  (if (null? (cdr set)) ; 要素が1個か
      (car set)
      (let ((first  (car set))   ; 頻度最少
            (second (cadr set))  ; 頻度2番めに少ない
            (rest   (cddr set))) ; 残りの要素
        (successive-merge (adjoin-set
                           (make-code-tree first second)
                           rest)))))

;; setが順序付けられているし、adjoin-setで追加する場合も順序付けられるので楽。




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ex2.70 ロック歌詞の圧縮

(define rock-lyrics
  '((A     2) (NA    16)
    (BOOM  1) (SHA    3)
    (GET   2) (YIP    9)
    (JOB   2) (WAH    1)))

(define rock-tree
  (generate-huffman-tree rock-lyrics))

;(print rock-tree)
;> ((leaf NA 16) ((leaf YIP 9) (((leaf A 2) ((leaf WAH 1) (leaf BOOM 1) (WAH BOOM) 2) (A WAH BOOM) 4) ((leaf SHA 3) ((leaf JOB 2) (leaf GET 2) (JOB GET) 4) (SHA JOB GET) 7) (A WAH BOOM SHA JOB GET) 11) (YIP A WAH BOOM SHA JOB GET) 20) (NA YIP A WAH BOOM SHA JOB GET) 36)
;> #<undef>

;;; see ./2-3-4/rock-tree.gif

(define rock-song
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))

(length rock-song)
;> 36
(* 3 (length rock-song))
;> 108 bit


(define compressed-rock (encode rock-song rock-tree))

(length compressed-rock)
;> 84 bits


;; ８文字アルファベットを固定長符号にする場合。
;; 2^3=8 なので符号ひとつにつき3bitあればOK。
;; 歌詞の記号数は36なので、固定長符号化した場合のビット数は
;;  36*3 = 108.

;; Huffman符号化したときのビット数は58。
;; 圧縮率は　0.78 程度
(/ 84 108.0)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ex2.71  1,2,4,...,2^(n-1)   n=5 n=10

(define n5-sample-set
  '((a 1) (b 2) (c 4) (d 8) (e 16)))

(define n10-sample-set
  '((a 1) (b 2) (c 4) (d 8) (e 16)
    (f 32) (g 64) (h 128) (i 256) (j 512)))

(define n5-huffman-tree (generate-huffman-tree n5-sample-set))
(define n10-huffman-tree (generate-huffman-tree n10-sample-set))

;;; 下で書いたユーティリティを使ってツリー表示
(huffman-tree->dot n5-huffman-tree)
(huffman-tree->dot n10-huffman-tree)

;; 最高頻度のビット数と最低頻度のビット数
;;
;; n=5
;; 最高頻度 1 bit (e)
;; 最低頻度 4 bit (a)
;;
;; n=10
;; 最高頻度 1 bit (j)
;; 最低頻度 9 bit (a)
;;
;; 一般の場合
;; 最高頻度 1 bit
;; 最低頻度 n-1 bit





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ex2.72 符号化ステップ数の増加頻度

;; 最高頻度の記号と最低頻度の記号を考察する

;; 最高頻度の記号
;; ルートからright-branchに行くだけで葉に行き着く
;; 記号リストの走査(element-of-set?)は2回実行される
;; ルート：記号リストは順序付けられていないから走査はｎステップ必要
;; 葉；記号リストは要素1個だけなので1ステップ必要
;;  総ステップ数　n+1 step
;;  O(n+1)＝O(n)


;; 最低頻度の記号
;; ルートから枝を経由して葉に行き着く
;; ルート：記号リストの走査はｎステップ
;; ルートからｋ番目の枝：記号リストのサイズはn-kなので走査はn-kステップ（1<=k<n-2）
;; 葉；記号リストは要素1個だけなので1ステップ必要
;; 　総ステップ数　1+2+...+n-1= n*(n-1)/2
;;   O(n*(n-1)/2) = O(n^2)


;; 符号化のステップ数
;;　最悪の場合でO(n^2)、最良の場合でO(n)
;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ユーティリティ
;;;; Huffman木をDot言語で出力する
;;;; dot -Tgif hoge.dot -o hoge.gif
;;;; 出力した文字列を次のような感じでGifに変換

;;; Huffman木をDot言語に変換する
(define (huffman-tree->dot tree)
  (print "digraph huffmanTree {")
  (tree->node tree #f #f)
  (print "}"))

;;; 木を走査してノードの情報を出力する
(define (tree->node tree parent-node bit)
  (let ((node (node-id tree))
        (label (with-output-to-string
                 (lambda () (display (symbols tree)))))
        (wt (number->string (weight tree))))
    (print node " [label=\"" label "\nweight:" wt "\"];")
    (if parent-node
        (print parent-node " -> " node "[label=\"" bit "\"];"))
    (if (not (leaf? tree))
        (begin
          (tree->node (left-branch tree) node "0")
          (tree->node (right-branch tree) node "1")))
    ))

;;; ノードのIDを返す
(define (node-id tree)
  (let ((syms (symbols tree)))
    (fold (lambda (a b) (string-append b "_" a))
          "N"
          (map symbol->string syms))))

(huffman-tree->dot sample-tree)
