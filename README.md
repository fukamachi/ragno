# Ragno

Common Lisp Web crawling library based on [Psychiq](https://github.com/fukamachi/psychiq).

<strong><span style="color:red">Warning</span>: This software is still ALPHA quality. The APIs will be likely to change.</strong>

## Usage

### 1. Writing a crawler class

```common-lisp
;; A part of 'my-crawlers' system.

(defclass rakugo-kyokai (ragno:crawler) ()
  (:default-initargs
   :request-delay 5
   :user-agent "Rakugo-Kyokai-Crawler"))

(defmethod ragno:parse ((crawler rakugo-kyokai) response)
  (let* ((uri (ragno:response-uri response))
         (path (quri:uri-path uri)))
    (cond
      ((string= "/broadcast/" path)
       (lquery:$ (lquery:initialize (ragno:response-body response))
         ".main .contents ul li a"
         (attr "href")
         (map (lambda (href)
                (psy:enqueue 'rakugo-kyokai
                             (list (quri:render-uri (quri:merge-uris (quri:uri href) uri))))))))
      ((string= "/jyoseki/index.php" path)
       (parse-jyoseki (ragno:response-body response)))
      (t ;; Unknown page
       nil)))))

(defun parse-jyoseki (html)
  (lquery:$ (lquery:initialize html)
    ".main .contents .member-detail"
    (combine "h2" ".time-table .yose")
    (map-apply (lambda (h2 table)
                 (format t "~A~%~{- ~A~%~}~%"
                         (lquery:$1 h2 (text))
                         (coerce (lquery:$ table
                                   "tr td.name a" (text)) 'list))))))
```

### 2. Enqueue a job

```common-lisp
(psy:enqueue 'rakugo-kyokai (list "http://rakugo-kyokai.jp/broadcast/"))
```

### 3. Start a worker process

```
$ psychiq --host localhost --port 6379 --system my-crawlers
```

## Worker options

- `:request-delay`: Interval seconds between for each job processes (Default: `0`)
- `:user-agent`: User-Agent header string when accessing web pages (Default: `"Ragno-Crawler"`)
- `:max-redirects`: Redirection limit when requesting web pages (Default: `5`)
- `:concurrency-per-domain`: Concurrency limit for each URL domains (Default: `1`)

## See Also

- [Psychiq](https://github.com/fukamachi/psychiq)
- [lQuery](https://github.com/Shinmera/lquery)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2017 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
