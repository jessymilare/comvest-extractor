;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :comvest-extractor-system
  (:use :cl :asdf))

(in-package :comvest-extractor-system)

(defsystem :comvest-extractor
  :name "Comvest Extractor"
  ; :version "0.0.1"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Extrai dados socioeconomicos do site da Comvest (Unicamp)"
  :components ((:file "comvest"))
  :depends-on (:alexandria :cl-ppcre :drakma :bordeaux-threads))
