; #lang racket

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
(define update-property (make-parameter nothing))

(command-line
  #:program "easyeffects-cli"
  #:once-each
  [("-i" "--input-preset") inp
    "Input preset to use" (input-preset inp)]
  [("-e" "--effect") eff
    "Effect identifier" (effect-identifier (just eff))]
  [("-s"  "--set") key value
    "Set an effect property" (update-property (just (cons key value)))]
  [("--enable")
    "Enable the effect" (update-property (just (cons "bypass" #f)))]
  [("--disable")
    "Disable the effect" (update-property (just (cons "bypass" #t)))]
)

(define (reload-preset preset)
  (system (string-append "easyeffects -l " preset)))

(define (update-input-effect preset effect update)
  (let* (
    [ source-preset   (read-source-preset preset) ]
    [ eff-sym         (string->symbol effect) ]
    [ update-key-sym      (string->symbol (car update)) ]
    [ pitch-path      (list 'input eff-sym update-key-sym) ]
    [ to-update-value (lambda (_) (cdr update)) ]
    [ updated-preset  (hash-update-path source-preset pitch-path to-update-value) ]
  )
    ; (println (hash-path updated-preset (list 'input 'pitch#0)))
    (write-output-preset output-preset-label updated-preset)
    (reload-preset output-preset-label)))

(define (main)
  (match (list (input-preset) (effect-identifier) (update-property))
    [(list preset (just effect) (just update)) #:when (not (eq? preset ""))
      (update-input-effect preset effect update)
      (println "Done the deed") ]

    [_ (println "Nothin to do")]))

(main)

