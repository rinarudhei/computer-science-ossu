;; simple-text-editor-starter.rkt

;  
;  In this problem, you will be designing a simple one-line text editor.
;  
;  
;  Your text editor should have the following functionality:
;  - when you type, characters should be inserted on the left side of the cursor 
;  - when you press the left and right arrow keys, the cursor should move accordingly  
;  - when you press backspace (or delete on a mac), the last character on the left of 
;    the cursors should be deleted
; 
; 
; 
; Constant:
; width
; height
; mts
; text color
; text size
; cursor image
; 
; Changing:
; display text
; cursor position
; 
; Big-bang options:
; to-draw
; on-key
; 



(require 2htdp/image)
(require 2htdp/universe)

;; A simple editor

;; Constants
;; =========

(define WIDTH 300)
(define HEIGHT 20)
(define MTS (empty-scene WIDTH HEIGHT))

(define CURSOR (rectangle 2 14 "solid" "red"))

(define TEXT-SIZE 14)
(define TEXT-COLOUR "black")

;; Data Definitions
;; ================

(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. pre is the text before the cursor, post is the text after
(define E0 (make-editor "" ""))
(define E1 (make-editor "a" ""))
(define E2 (make-editor "" "b"))

#;
(define (fn-for-editor e)
  (... (editor-pre e)
       (editor-post e)))

;; Editor -> Editor
;; start the world with a empty scene (MTS): (main (make-editor "" ""))
(define (main e)
  (big-bang e                   ; Editor
    (to-draw   render)          ; Editor -> Image
    (on-key    handle-key)))    ; Editor KeyEvent -> Editor


;; Editor -> Image
;; render pre cursor and post from left to right. 
;;
(check-expect (render E0) (overlay/align "left"
                                         "middle"
                                         (beside (text (editor-pre E0) TEXT-SIZE TEXT-COLOUR)
                                                 CURSOR
                                                 (text (editor-post E0) TEXT-SIZE TEXT-COLOUR))
                                         MTS))


(check-expect (render E1) (overlay/align "left"
                                         "middle"
                                         (beside (text (editor-pre E1) TEXT-SIZE TEXT-COLOUR)
                                                 CURSOR
                                                 (text (editor-post E1) TEXT-SIZE TEXT-COLOUR))
                                         MTS))
                                  

(check-expect (render E2) (overlay/align "left"
                                         "middle"
                                         (beside (text (editor-pre E2) TEXT-SIZE TEXT-COLOUR)
                                                 CURSOR
                                                 (text (editor-post E2) TEXT-SIZE TEXT-COLOUR))
                                         MTS))


;(define (render e) MTS) ; stub
;; <use template from Editor>
(define (render e)
  (overlay/align "left" "middle" (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOUR)
                                         CURSOR
                                         (text (editor-post e) TEXT-SIZE TEXT-COLOUR))
                 MTS))

;; Editor KeyEvent -> Editor
;; insert character from pressed key to the editor-pre property
;; handle delete key to delete the last character in editor-pre property only when editor-pre has length more than 0
;; handle left key to move the last character in "pre" to the first character in "post", do nothing if cursor is on the leftest
;; handle right key to move the first character in "post" to the last character in "pre", do nothing if cursor is on the rightest
;; other than "left" "right" and "\b", only handle KeyEvent with string-length equal 1

(check-expect (handle-key E0 "a") (make-editor "a" ""))
(check-expect (handle-key (make-editor "abc" "") "d") (make-editor "abcd" ""))
(check-expect (handle-key (make-editor "abcd" "") "left") (make-editor "abc" "d"))
(check-expect (handle-key (make-editor "abc" "d") "d") (make-editor "abcd" "d"))
(check-expect (handle-key (make-editor "abcd" "d") "right") (make-editor "abcdd" ""))

(check-expect (handle-key (make-editor "abcd" "") "right") (make-editor "abcd" ""))
(check-expect (handle-key (make-editor "" "abcd") "left") (make-editor "" "abcd"))

(check-expect (handle-key (make-editor "abcdd" "") "\b") (make-editor "abcd" ""))
(check-expect (handle-key (make-editor "" "asdf") "\b") (make-editor "" "asdf"))

(check-expect (handle-key (make-editor "" "abcd") "insert") (make-editor "" "abcd"))


;(define (handle-key e ke) (make-editor "" "")) ;stub

(define (handle-key e ke)
  (cond [(limit? e ke) e]
        [(key=? ke  "left") (make-editor (but-last (editor-pre e)) (string-append (get-last (editor-pre e)) (editor-post e)))]
        [(key=? ke "right") (make-editor (string-append (editor-pre e) (get-first (editor-post e))) (but-first (editor-post e)))]
        [(key=? ke    "\b") (make-editor (but-last (editor-pre e)) (editor-post e))]
        [(= 1 (string-length ke))
         (make-editor (string-append (editor-pre e) ke) (editor-post e))]
        [else e]))

;; Editor KeyEvent -> Boolean
;; return true if key pressed is "\b" and editor-pre is ""
;; return true if key pressed is "left" and editor-pre is ""
;; return true if key pressed is "right" and editor-post is ""
(check-expect (limit? (make-editor "" "asdf") "\b") true)
(check-expect (limit? (make-editor "a" "asdf") "\b") false)

(check-expect (limit? (make-editor "" "asdf") "left") true)
(check-expect (limit? (make-editor "a" "asdf") "left") false)

(check-expect (limit? (make-editor "asdf" "") "right") true)
(check-expect (limit? (make-editor "asdf" "a") "right") false)

;(define (limit? e ke) false) ;stub

; use template from Editor
(define (limit? e ke)
  (or (and (string=? (editor-pre e) "") (string=? ke "\b"))
      (and (string=? (editor-pre e) "") (string=? ke "left"))
      (and (string=? (editor-post e) "") (string=? ke "right"))))

;; String -> String
;; Consume string and return it but not the last character
(check-expect (but-last "asdf") "asd")
(check-expect (but-last "a") "")
(check-expect (but-last "") "")

;(define (but-last s) "") ;stub
;; Template rules used:
;; atomic non distinct: String

#;
(define (but-last s) ... s) 

(define (but-last s)
  (if (> (string-length s) 0) (substring s 0 (- (string-length s) 1)) s))

;; String -> String
;; Consume string and return it but not the first character
(check-expect (but-first "asdf") "sdf")
(check-expect (but-first "a") "")
(check-expect (but-first "") "")

;(define (but-first s) "") ;stub
;; Template rules used:
;; atomic non distinct: String

#;
(define (but-first s) ... s) 

(define (but-first s)
  (if (> (string-length s) 0) (substring s 1) s))


;; String -> String
;; return last character of the String input
(check-expect (get-last "asdf") "f")
(check-expect (get-last "f") "f")
(check-expect (get-last "") "")

;(define (last s) "") ;stub
;; Template rules used:
;; atomic non distinct: String

#;
(define (last s) ... s)

(define (get-last s)
  (if (> (string-length s) 0) (substring s (- (string-length s) 1)) s))


;; String -> String
;; return first character of the String input
(check-expect (get-first "asdf") "a")
(check-expect (get-first "f") "f")
(check-expect (get-first "") "")

;(define (get-first s) "") ;stub
;; Template rules used:
;; atomic non distinct: String

#;
(define (get-first s) ... s)

(define (get-first s)
  (if (> (string-length s) 0) (substring s 0 1) s)) 



