#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (fs fatfs))

(define (ls/R x path)
  (let ((rev-path (reverse path)))
    (for-each (lambda (name)
                (let ((full-name (reverse (cons name rev-path))))
                  (write full-name)
                  (let ((stat (fatfs-stat x full-name)))
                    (display #\space)
                    (display (cdr (assq 'file-length stat)))
                    (newline)
                    (when (cdr (assq 'dir? stat))
                      (ls/R x full-name)))))
              (fatfs-directory-list x path))))

(let ((x (open-fatfs (open-file-input-port "test.img"))))
  (ls/R x '())
  (fatfs-directory-list x '())
  (let ((port (transcoded-port (fatfs-open-file x '("README.md"))
                               (native-transcoder))))
    (write port)
    (newline)

    (let lp ()
      (let ((line (get-line port)))
        (write line)
        (newline)
        (unless (eof-object? line)
          (lp))))))

(flush-output-port (current-output-port))
