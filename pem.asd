(defsystem "pem"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "PEM parser"
  :depends-on ("asn1" "cl-ppcre" "optima" "ironclad" "cl-base64" "pem/main"))
