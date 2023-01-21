; #lang racket

(require json)
(require racket/string)
 (require racket/system)

; TODO: Handle XDG_CONFIG_HOME not set
(define (get-preset-path source)
  (string-append* (list
    (getenv "XDG_CONFIG_HOME")
    "/easyeffects/input/"
    source
    ".json")))

(define (read-source-preset source)
  (let (
    [ source-preset-path (get-preset-path source) ]
  )
    (string->jsexpr (file->string source-preset-path))))

(define (write-output-preset name preset)
  (let (
    [ preset-path (get-preset-path name) ]
  )
    (display-to-file (jsexpr->string preset) preset-path #:exists 'truncate/replace)
  ))

(define (hash-path hash path)
  (let* (
    ; [ with-default (lambda (v f)
    ;   (if (eq? v 'null) def (f v))) ]

    [ reducer (lambda (k h) (hash-ref h k)) ]

    [ result (foldl reducer hash path) ]
  )
  result))

(define (hash-update-path hash path updater)
  (match path
    [ (list)  (updater hash) ]
    [ (list key tl ...) (hash-update hash key (lambda (val)
        (hash-update-path val tl updater)
      )) ]
  ))

(define output-preset-label "__output__")

(define (main)
  (let* (
    [ source-preset (read-source-preset "mic2") ]
    [ pitch-path (list 'input 'pitch#0 'bypass) ]
    [ pitch-value (hash-path source-preset pitch-path) ]
    [ updated-preset (hash-update-path source-preset pitch-path (lambda (_) #t)) ]
  )

    (write-output-preset output-preset-label updated-preset)
    (system (string-append* (list "easyeffects -l " output-preset-label)))
    (println "Aaayayy donezo")
  ))

; Write to output preset json
; Reload output preset

