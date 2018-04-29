;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

;;; FAT filesystem

;; Supports FAT12, FAT16, FAT32 and VFAT.

;; TODO:

;; Write support:
;;   Allocate clusters
;;   Create directory entries
;;   Deallocate clusters
;;   Delete files
;; Complete stat support

(library (fs fatfs)
  (export
    open-fatfs
    fatfs-directory-list
    fatfs-stat
    fatfs-open-file)
  (import
    (rnrs (6))
    (struct pack))

(define DIRENT-READ-ONLY #b00000001)
(define DIRENT-HIDDEN    #b00000010)
(define DIRENT-SYSTEM    #b00000100)
(define DIRENT-VOLUME-ID #b00001000)
(define DIRENT-DIRECTORY #b00010000)
(define DIRENT-ARCHIVE   #b00100000)

(define-record-type fatfs
  (sealed #t)
  (nongenerative fatfs-v0-34b32cf0-e410-48a0-875d-a178a45a06a0)
  (fields dev type bytes/sector sectors/cluster
          FATs
          first-FAT-sector
          first-data-sector
          root-directory-cluster        ;FAT32 only
          root-directory-sectors
          root-directory-entries
          codepage))

(define dev-copy-raw-sector
  (case-lambda
    ((dev idx size)
     (set-port-position! dev (fx* idx size))
     (get-bytevector-n dev size))
    ((dev idx size sectors)
     (set-port-position! dev (fx* idx size))
     (get-bytevector-n dev (* size sectors)))))

(define cp437
  "\x2007;☺☻♥♦♣♠•◘○◙♂♀♪♫☼►◄↕‼¶§▬↨↑↓→←∟↔▲▼ !\"#$%&'()*+,-./0123456789:;<=>?\
@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~⌂\
ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒáíóúñÑªº¿⌐¬½¼¡«»░▒▓│┤╡╢╖╕╣║╗╝╜╛┐└\
┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀αβΓπΣσµτΦΘΩδ∞∅∈∩≡±≥≤⌠⌡÷≈°∙·√ⁿ²■\xA0;")

(define open-fatfs
  (case-lambda
    ((dev)
     (open-fatfs dev cp437))
    ((dev codepage)
     ;; Read the BPB block and parse the data. TODO: try the backup at
     ;; sector 6 if this fails.
     (let ((blk0 (dev-copy-raw-sector dev 0 512)))
       (unless (= #xAA55 (unpack "<S" blk0 510))
         (error 'open-fatfs "Missing #xAA55 signature" dev))
       (let-values
           (((bytes/sector
              sectors/cluster reserved-sectors FATs root-directory-entries
              total-sectors _media-descriptor-type sectors/FAT
              _sectors/track _heads _hidden-sectors large-total-sectors)
             (unpack "<u11x SCSCSSCSSSLL" blk0))
            ;; FAT12/16
            ((_fat12-drive-number _fat12-flags fat12-signature fat12-serial)
             (unpack "<u36x CCCL 11x 8x" blk0))
            ;; FAT32
            ((fat32-sectors/FAT
              _fat32-flags _fat32-version fat32-root-dir-cluster fat32-fsinfo-sector
              _fat32-backup-boot-sector _fat32-drive-number _fat32-nt-flags fat32-signature
              fat32-serial)
             (unpack "<u36x LSSLSS 12x CCCL 11x 8x" blk0)))
         ;; Compute the parameters needed during operation.
         (let* ((total-sectors (if (zero? total-sectors) large-total-sectors total-sectors))
                (sectors/FAT (if (zero? sectors/FAT) fat32-sectors/FAT sectors/FAT))
                (root-directory-sectors
                 (div (+ (* 32 root-directory-entries) (- bytes/sector 1))
                      bytes/sector))
                (first-data-sector
                 (+ reserved-sectors (* sectors/FAT FATs) root-directory-sectors))
                (first-FAT-sector reserved-sectors)
                (total-data-sectors
                 (- total-sectors reserved-sectors (* sectors/FAT FATs)
                    root-directory-sectors))
                (total-clusters (div total-data-sectors sectors/cluster))
                (type (cond ((< total-clusters 4085) 'fat12)
                            ((< total-clusters 65525) 'fat16)
                            ((< total-clusters 268435445) 'fat32)
                            (else 'exfat)))
                (fs (make-fatfs dev type bytes/sector sectors/cluster FATs
                                first-FAT-sector
                                first-data-sector
                                fat32-root-dir-cluster
                                root-directory-sectors
                                root-directory-entries
                                codepage)))
           (unless (memv (case type
                           ((fat12 fat16) fat12-signature)
                           (else fat32-signature))
                         '(#x28 #x29))
             (error 'open-fat-filesystem "Invalid FAT signature" dev))
           fs))))))

(define (make-fat12-root-directory-reader fatfs)
  (define sector
    (fx- (fatfs-first-data-sector fatfs)
         (fatfs-root-directory-sectors fatfs)))
  (define last-sector (fx+ sector (fatfs-root-directory-sectors fatfs)))
  (lambda ()
    (cond
      ((fx>=? sector last-sector)
       (eof-object))
      (else
       (let ((s (dev-copy-raw-sector (fatfs-dev fatfs) sector
                                     (fatfs-bytes/sector fatfs))))
         (set! sector (+ sector 1))
         s)))))

(define (make-root-directory-reader fatfs)
  (case (fatfs-type fatfs)
    ((fat12 fat16)
     (make-fat12-root-directory-reader fatfs))
    (else
     (make-cluster-reader fatfs (fatfs-root-directory-cluster fatfs)))))

(define (utf16z->string bv)             ;XXX: USC-2 really
  (let ((len (do ((i 0 (fx+ i 2)))
                 ((or (fx=? i (bytevector-length bv))
                      (fxzero? (bytevector-u16-native-ref bv i)))
                  (fxdiv i 2)))))
    (substring (utf16->string bv (endianness little) #t)
               0 len)))

(define (fatfs-directory-lister fatfs reader)
  (define (entry-short-filename sector offset downcase-filename downcase-extension)
    (call-with-string-output-port
      (lambda (p)
        (let lp ((i 0))
          (unless (fx=? i 11)
            (let ((b (bytevector-u8-ref sector (fx+ offset i))))
              (when (and (fx=? i 8)
                         (not (= #x20 (bytevector-u8-ref sector (fx+ offset 8)))))
                (put-char p #\.))
              (cond
                ((and (fx=? i 0) (fx=? b #x05))
                 ;; #xe5 is used to mark unused/deleted entries, so
                 ;; this is the replacement character.
                 (put-char p (string-ref (fatfs-codepage fatfs) #xe5)))
                (else
                 (unless (fx=? b #x20)
                   (let* ((c (string-ref (fatfs-codepage fatfs) b))
                          (c (if (or (and downcase-filename (fx<? i 8))
                                     (and downcase-extension (fx>=? i 8)))
                                 (char-downcase c)
                                 c)))
                     (put-char p c)))
                 (lp (fx+ i 1))))))))))
  (define (entry-checksum sector offset)
    (do ((i 0 (fx+ i 1))
         (checksum 0 (fxand #xff (fx+ (bytevector-u8-ref sector (fx+ offset i))
                                      (fxrotate-bit-field checksum 0 8 7)))))
        ((fx=? i 11)
         checksum)))
  (define sector (reader))
  (define offset 0)
  (define lfn-buf (make-bytevector 520 0))
  (lambda ()                         ;reads a complete directory entry
    (let lp-sector ((lfn-expect #f) (lfn-check #f))
      (cond
        ((eof-object? sector)
         (values (eof-object) #f #f #f #f))
        (else
         (let lp-dirent ((lfn-expect lfn-expect) (lfn-check lfn-check))
           (cond
             ((fx=? offset (bytevector-length sector))
              (set! sector (reader))
              (set! offset 0)
              (lp-sector lfn-expect lfn-check))
             ((fx=? (bytevector-u8-ref sector offset) 0) ;end of dir
              (values (eof-object) #f #f #f #f))
             (else
              (let ((attr (bytevector-u8-ref sector (fx+ offset 11))))
                (cond
                  ((fx=? (bytevector-u8-ref sector offset) #xe5) ;deleted
                   (set! offset (fx+ offset 32))
                   (lp-dirent #f #f))
                  ((and (fx=? attr #x0f)
                        (fx=? (bytevector-u8-ref sector (fx+ offset 12)) #x00)) ;entry type
                   ;; Long filenames.
                   (let-values (((seq check) (unpack "C12xC" sector offset)))
                     (let ((idx (fxbit-field seq 0 5)) (last? (fxbit-set? seq 6)))
                       (cond
                         ;; Some sanity checks: entries are in
                         ;; decreasing sequence and the checksum is
                         ;; the same as the previous entry.
                         ((and (or (not lfn-check) (fx=? lfn-check check))
                               (or last? (fx=? idx lfn-expect)))
                          (when last?
                            (bytevector-fill! lfn-buf 0))
                          (let ((buf-offset (fx* (fx- idx 1) (* 13 2))))
                            (bytevector-copy! sector (fx+ offset 1)
                                              lfn-buf buf-offset
                                              10)
                            (bytevector-copy! sector (fx+ offset 14)
                                              lfn-buf (fx+ buf-offset 10)
                                              12)
                            (bytevector-copy! sector (fx+ offset 28)
                                              lfn-buf (fx+ buf-offset 22)
                                              4))
                          (set! offset (fx+ offset 32))
                          (lp-dirent (fx- idx 1) check))
                         (else          ;ignore an invalid entry
                          (set! offset (fx+ offset 32))
                          (lp-dirent #f #f))))))
                  ((not (fxzero? (fxand attr DIRENT-VOLUME-ID))) ;volume id
                   (set! offset (fx+ offset 32))
                   (lp-dirent #f #f))
                  (else
                   (let*-values (((cases cluster-high cluster-low file-length)
                                  (unpack "<11x x C 7xS 2x2x S L" sector offset))
                                 ((fn) (entry-short-filename sector offset
                                                             (fxbit-set? cases 3)
                                                             (fxbit-set? cases 4))))
                     (let ((cluster (bitwise-ior
                                     (bitwise-arithmetic-shift-left cluster-high 16)
                                     cluster-low))
                           (dir? (not (fxzero? (fxand attr DIRENT-DIRECTORY))))
                           (lfn (and (eqv? lfn-expect 0)
                                     (fx=? lfn-check (entry-checksum sector offset))
                                     (utf16z->string lfn-buf))))
                       (set! offset (fx+ offset 32))
                       (values fn lfn cluster file-length dir?))))))))))))))

(define (fatfs-directory-list fatfs dir-path)
  (let lp-dir ((dir-reader (fatfs-directory-lister fatfs (make-root-directory-reader fatfs)))
               (path dir-path))
    (cond
      ((null? path)                     ;found the directory
       (let lp ()
         (let-values (((fn lfn . _) (dir-reader)))
           (cond ((eof-object? fn)
                  '())
                 ((member fn '("." ".."))
                  (lp))
                 (else
                  (cons (or lfn fn) (lp)))))))
      (else                             ;find a subdirectory
       (let lp ()
         (let-values (((fn lfn cluster _ dir?) (dir-reader)))
           (cond ((eof-object? fn)
                  (error 'fatfs-directory-list "Directory not found" dir-path))
                 ((or (string-ci=? fn (car path))
                      (and lfn (string-ci=? lfn (car path))))
                  (unless dir?
                    (error 'fatfs-directory-list "Not a directory" dir-path))
                  (lp-dir (fatfs-directory-lister fatfs (make-cluster-reader fatfs cluster))
                          (cdr path)))
                 (else (lp)))))))))

(define (fatfs-stat fatfs file-path)
  (let lp-dir ((dir-reader (fatfs-directory-lister fatfs (make-root-directory-reader fatfs)))
               (path file-path))
    (cond
      ((null? (cdr path))               ;found the directory
       (let lp ()
         (let-values (((fn lfn _ file-length dir?) (dir-reader)))
           (cond ((eof-object? fn)
                  (error 'fatfs-stat "File not found" file-path))
                 ((or (string-ci=? fn (car path))
                      (and lfn (string-ci=? lfn (car path))))
                  (list (cons 'file-length file-length) (cons 'dir? dir?)))
                 (else
                  (lp))))))
      (else                             ;find a subdirectory
       ;; XXX: copied from fatfs-directory-list
       (let lp ()
         (let-values (((fn lfn cluster _ dir?) (dir-reader)))
           (cond ((eof-object? fn)
                  (error 'fatfs-stat "Directory not found" file-path))
                 ((or (string-ci=? fn (car path))
                      (and lfn (string-ci=? lfn (car path))))
                  (unless dir?
                    (error 'fatfs-stat "Not a directory" file-path))
                  (lp-dir (fatfs-directory-lister fatfs (make-cluster-reader fatfs cluster))
                          (cdr path)))
                 (else (lp)))))))))

(define (fatfs-open-file fatfs file-path)
  ;; XXX: file-path is an absolute path as a list with no .. or . components
  (let lp ((path file-path)
           (dir-reader (fatfs-directory-lister fatfs (make-root-directory-reader fatfs))))
    (let-values (((fn lfn cluster file-length dir?) (dir-reader)))
      (cond
        ((eof-object? fn)
         (error 'fatfs-open-file "File not found" file-path))
        ((or (string-ci=? fn (car path))
             (and lfn (string-ci=? lfn (car path))))
         (cond
           (dir?
            (when (null? (cdr path))
              (error 'fatfs-open-file "Name is a directory" file-path))
            (lp (cdr path)
                (fatfs-directory-lister fatfs (make-cluster-reader fatfs cluster))))
           (else
            ;; FIXME: can copy data more efficiently into the reader's
            ;; memory, can also truncate at the end of file
            (make-fatfs-custom-port fatfs cluster file-length
                                    (string-append "fatfs:" (or lfn fn))))))
        (else
         (lp path dir-reader))))))

(define (get-next-cluster fatfs cluster)
  (case (fatfs-type fatfs)
    ((fat12)
     (let*-values (((sector-size) (fatfs-bytes/sector fatfs))
                   ((fat-offset) (+ cluster (div cluster 2)))
                   ((entry-sector entry-offset) (div-and-mod fat-offset sector-size))
                   ((FAT) (dev-copy-raw-sector (fatfs-dev fatfs)
                                               ;; TODO: Use second FAT if this fails.
                                               (+ (fatfs-first-FAT-sector fatfs)
                                                  entry-sector)
                                               sector-size
                                               2))) ;entry may live across two sectors
       (let* ((value (unpack "<uS" FAT entry-offset))
              (value (if (fxodd? cluster)
                         (fxarithmetic-shift-right value 4)
                         (fxand value #xfff))))
         (cond ((or (fx>=? value #xff8) (fx<=? value #x001))
                (eof-object))
               ((fx=? value #xff7)
                (error 'get-next-cluster "Next cluster is marked as bad" cluster))
               (else
                value)))))
    ((fat16)
     (let*-values (((sector-size) (fatfs-bytes/sector fatfs))
                   ((fat-offset) (* cluster 2))
                   ((entry-sector entry-offset) (div-and-mod fat-offset sector-size))
                   ((FAT) (dev-copy-raw-sector (fatfs-dev fatfs)
                                               (+ (fatfs-first-FAT-sector fatfs)
                                                  entry-sector)
                                               sector-size)))
       (let ((value (unpack "<uS" FAT entry-offset)))
         (cond ((or (fx>=? value #xfff8) (fx<=? value #x0001))
                (eof-object))
               ((fx=? value #xfff7)
                (error 'get-next-cluster "Next cluster is marked as bad" cluster))
               (else
                value)))))
    ((fat32)
     (let*-values (((sector-size) (fatfs-bytes/sector fatfs))
                   ((fat-offset) (* cluster 4))
                   ((entry-sector entry-offset) (div-and-mod fat-offset sector-size))
                   ((FAT) (dev-copy-raw-sector (fatfs-dev fatfs)
                                               (+ (fatfs-first-FAT-sector fatfs)
                                                  entry-sector)
                                               sector-size)))
       (let ((value (bitwise-and (unpack "<uL" FAT entry-offset) #xfffffff)))
         (cond ((or (fx>=? value #xffffff8) (fx<=? value #x0000001))
                (eof-object))
               ((fx=? value #xffffff7)
                (error 'get-next-cluster "Next cluster is marked as bad" cluster))
               (else
                value)))))
    (else
     (error 'get-next-cluster "TODO: Unsupported fs type" (fatfs-type fatfs)))))

(define (make-cluster-reader fatfs cluster)
  (define cluster-sector 0)
  (lambda ()
    (cond
      ((or (eof-object? cluster) (eqv? cluster 0))
       (eof-object))
      (else
       (let ((data (dev-copy-raw-sector (fatfs-dev fatfs)
                                        (+ (fatfs-first-data-sector fatfs)
                                           (* (- cluster 2) (fatfs-sectors/cluster fatfs))
                                           cluster-sector)
                                        (fatfs-bytes/sector fatfs))))
         (cond ((fx=? cluster-sector (fx- (fatfs-sectors/cluster fatfs) 1))
                (set! cluster (get-next-cluster fatfs cluster))
                (set! cluster-sector 0))
               (else
                (set! cluster-sector (fx+ cluster-sector 1))))
         data)))))

(define (make-fatfs-custom-port fatfs cluster file-length filename)
  ;; State
  (define file-position 0)
  (define sector-position 0)
  (define sector-data #vu8())
  (define reader (make-cluster-reader fatfs cluster))
  ;; Custom port
  (define id filename)
  (define (read! bv start count)
    (when (fx=? sector-position (bytevector-length sector-data))
      ;; Buffer up to one sector. This can sometimes read directly into bv.
      (set! sector-data (reader))
      (set! sector-position 0))
    (if (eof-object? sector-data)
        0
        (let* ((remaining-bytes (- file-length file-position))
               (bytes-read (fxmin count
                                  (fx- (bytevector-length sector-data)
                                       sector-position)
                                  remaining-bytes)))
          (bytevector-copy! sector-data sector-position
                            bv start bytes-read)
          (set! sector-position (fx+ sector-position bytes-read))
          (set! file-position (+ file-position bytes-read))
          bytes-read)))
  ;; (define (write! bv start count)
  ;;   #f)
  (define (get-position)
    file-position)
  ;; (define (set-position! pos)
  ;;   #f)
  (define set-position! #f)
  (define close #f)
  (make-custom-binary-input-port
   id read! #;write! get-position set-position! close)))
