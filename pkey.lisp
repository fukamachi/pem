(defpackage #:pem/pkey
  (:use #:cl)
  (:import-from #:pem/parser
                #:parse-file)
  (:import-from #:asn1
                #:decode
                #:rsa-public-key-info)
  (:import-from #:trivia
                #:match
                #:access)
  (:import-from #:cl-base64
                #:base64-string-to-usb8-array)
  (:import-from #:ironclad)
  (:export #:read-from-file
           #:read-pem))
(in-package #:pem/pkey)

(defun read-public-key (key)
  (let* ((der (base64:base64-string-to-usb8-array key))
         (der (asn1:decode der)))
    (match der
      ((asn1:rsa-public-key-info n e)
       (ironclad:make-public-key :rsa :n n :e e))
      (otherwise (error "Unexpected format: ~S" key)))))

(defun read-private-key (key)
  (let* ((der (base64:base64-string-to-usb8-array key))
         (der (asn1:decode der)))
    (match der
      ((asn1:rsa-private-key :private-exponent d :modulus n)
       (ironclad:make-private-key :rsa :d d :n n))
      (otherwise (error "Unexpected format: ~S" key)))))

(defun read-pkcs8-private-key (key)
  "Returns an IRONCLAD:RSA-PRIVATE-KEY from a PKCS#8 private key.
Only support RSA private keys."
  (let* ((der (base64:base64-string-to-usb8-array key))
         (der (asn1:decode der)))
    (match der
      ((list
        (list*
         :sequence
         (list (cons :integer 0)      ; RFC5208: Version, must be 0
               (cons
                :sequence
                (assoc
                 :object-identifier   ; RFC5208: PrivKeyAlgId = RSA encryption OID
                 #(1 2 840 113549 1 1 1)))
               (cons
                :octet-string         ; RFC5208: encoded private key
                (access
                 #'asn1:decode
                 (asn1/format/rsa::rsa-private-key
                  :private-exponent d
                  :modulus n))))))
       (ironclad:make-private-key :rsa :d d :n n))
      (otherwise (error "Unexpected format: ~S" key)))))


(defun read-key (data)
    (let ((public-key (cdr (assoc "PUBLIC KEY" data :test #'string=)))
        (private-key (cdr (assoc "RSA PRIVATE KEY" data :test #'string=)))
        (pkcs8-private-key (cdr (assoc "PRIVATE KEY" data :test #'string=))))
      (cond
        (public-key (read-public-key public-key))
      (private-key (read-private-key private-key))
      (pkcs8-private-key (read-pkcs8-private-key pkcs8-private-key)))))

(defun read-from-file (pem)
  (let ((data (pem/parser:parse-file pem)))
    (read-key data)))

(defgeneric read-pem (pem)
  (:method ((pem pathname))
    (read-from-file pem))
  (:method ((pem stream))
    (let ((data (pem/parser:parse pem)))
      (read-key data)))
  (:method ((pem string))
    (read-pem (make-string-input-stream pem))))
