(use srfi-1)
(use util.list)

(define *undefined* (if #f #f))

(define (list< a b) (< (car a) (car b)))

;;
;; 重複要素を排除したリストを作る
;;
(define (list->lset_lset lis)
  (apply lset-adjoin eq? '() lis))

(define (list->lset_sort_uniq lis)
  (define (cmp< a b) (< (eq-hash a) (eq-hash b)))
  (uniq eq? (sort lis cmp<)))

(define (list->lset_hashtable lis)
  (let1 ht (make-hash-table 'eq?)
	(for-each (cut hash-table-put! ht <> #t) lis)
	(hash-table-map ht (lambda (k v) k))))

(define list->lset list->lset_hashtable)
;;
;; ２つのリストがそれぞれ重複要素をもたず、かつ集合として等価な場合に#t
;;
(define (lset-eq? list1 list2)
  (and (= (length (list->lset list1)) (length list1) (length list2))
	   (lset= eq? list1 list2)))
(define (lset-eqv? list1 list2)
  (and (= (length (list->lset list1)) (length list1) (length list2))
	   (lset= eqv? list1 list2)))
(define (lset-equal? list1 list2)
  (and (= (length (list->lset list1)) (length list1) (length list2))
	   (lset= equal? list1 list2)))
;(equal? (list->lset list1) (list->lset list2))

(define (uniq elt= lis)
  (let loop ((last (if #f #f)) (rest lis) (result '()))
	(if (null? rest) (reverse! result)
		(let1 top (car rest)
		  (if (elt= top last)
			  (loop last (cdr rest) result)
			  (loop top (cdr rest) (cons top result)) )))))

;;
;; 複数のシンボルをくっつけて１つのシンボルにする
;;
(define (symbol-append . syms)
  (string->symbol (apply string-append (map symbol->string syms))))

(define (symbol-length sym)
  (string-length (symbol->string sym)))

;;
;; シンボル tag でタグ付けされているかを調べる関数をつくる
;;
(define (tagged-list?-proc tag)
  (lambda (exp) (if (pair? exp) (eq? tag (car exp)) #f)))

;;
;; リスト lis をあるシンボル sym で分割する
;;
(define (split-list-by-symbol lis sym)
  (let loop ((rest lis) (cur '()) (result '()))
	(if (null? rest)
		(reverse! (cons (reverse! cur) result))
		(let1 r (car rest)
		  (if (eq? sym r)
			  (loop (cdr rest) '() (cons (reverse! cur) result))
			  (loop (cdr rest) (cons r cur) result)
			  )))))

;;
;; リストの中で、assocで
;;
(define (collect-assq obj alist)
  (filter (lambda (a) (eq? (car a) obj)) alist))
(define (collect-cdr-assq obj alist) (map cdr (collect-assq obj alist)))

(define (collect-assoc obj alist)
  (filter (lambda (a) (equal? (car a) obj)) alist))
(define (collect-cdr-assoc obj alist) (map cdr (collect-assoc obj alist)))

(define (!collect-assq obj alist)
  (filter (lambda (a) (not (eq? (car a) obj))) alist))
;(define (!collect-assq-cdr obj alist) (map cdr (!collect-assq obj alist)))
