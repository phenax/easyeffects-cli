#!/usr/bin/env racket
#lang racket

(require json)
(require racket/string)
(require racket/system)
(require data/maybe)

; TODO: Handle XDG_CONFIG_HOME not set
(define (get-preset-path source)
  (string-append* (list
    (getenv "XDG_CONFIG_HOME")
    "/easyeffects/input/"
    source
    ".json")))

(define (read-source-preset source)
  (let ([ source-preset-path (get-preset-path source) ])
  (string->jsexpr (file->string source-preset-path))))

(define (write-output-preset name preset)
  (let ([ preset-path (get-preset-path name) ])
  (display-to-file
    (jsexpr->string preset)
    preset-path
    #:exists 'truncate/replace)))

(define (reload-preset preset)
  (system (string-append "easyeffects -l " preset)))

(define (hash-path hash path)
  (let* ([ reducer (lambda (k h) (hash-ref h k)) ])
  (foldl reducer hash path)))

(define (hash-update-path hash path updater)
  (match path
    [ (list)  (updater hash) ]
    [ (list key tl ...) (hash-update hash key (lambda (val)
        (hash-update-path val tl updater)
      )) ]
  ))

(define output-preset-label "__output__")

(define input-preset (make-parameter output-preset-label))
(define effect-identifier (make-parameter nothing))
(define update-properties (make-parameter '()))

(define (addupdate k v)
  (update-properties (cons (cons k v) (update-properties))))

(define (update-input-effect preset effect update)
  (let* (
    [ source-preset   (read-source-preset preset) ]
    [ eff-sym         (string->symbol effect) ]
    [ update-key-sym      (string->symbol (car update)) ]
    [ pitch-path      (list 'input eff-sym update-key-sym) ]
    [ to-update-value (lambda (_) (cdr update)) ]
    [ updated-preset  (hash-update-path source-preset pitch-path to-update-value) ]
  )
    (write-output-preset output-preset-label updated-preset)
    (reload-preset output-preset-label)))

(define (show-input-effect preset effect)
  (let* (
    [ source-preset   (read-source-preset preset) ]
    [ eff-sym         (string->symbol effect) ]
    [ pitch-path      (list 'input eff-sym) ]
    [ effect          (jsexpr->string (hash-path source-preset pitch-path)) ]
  )
    (displayln effect)))

; Parse command line args and update parameters
(command-line
  #:program "easyeffects-cli"

  #:once-each
  [("-i" "--input-preset") inp
    "Input preset to use" (input-preset inp)]
  [("-e" "--effect") eff
    "Effect identifier" (effect-identifier (just eff))]
  [("--enable")
    "Enable the effect" (addupdate "bypass" #f)]
  [("--disable")
    "Disable the effect" (addupdate "bypass" #t)]

  #:multi
  [("-s"  "--set") key value
    "Set an effect property" (addupdate key value)]
)

(define (main)
  (match (list (input-preset) (effect-identifier) (update-properties))
    [(list preset (just effect) '()) #:when (not (eq? preset ""))
      (displayln (string-append "> JSON config for " preset "." effect "\n"))
      (show-input-effect preset effect) ]
    [(list preset (just effect) updates) #:when (not (eq? preset ""))
      (map (curry update-input-effect preset effect) updates)
      (displayln "Updated") ]
    [_ (displayln "Nothin to do")]))

(main)

