;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright 2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :comvest-extractor
  (:use :cl :cl-ppcre :drakma :alexandria)
  (:nicknames :com)
  (:export #:dados-comvest #:dados-comvest-arquivos))

(in-package :comvest-extractor)

(define-constant +inner-tag+
      '(:non-greedy-repetition 0 nil
        (:alternation :word-char-class :whitespace-char-class #\' #\= #\. #\"))
    :test 'equal)

(defun regex-opcoes-comvest (nome)
  `(:sequence
    "<select name=\"" ,nome
    ,+inner-tag+
    "\">"
    (:non-greedy-repetition
     0 nil (:alternation :everything :whitespace-char-class))
    "</select>"))

(defparameter *regex-opcoes-comvest-single*
  `(:sequence
    "<option value=\""
    (:register (:non-greedy-repetition 0 nil
                (:alternation :word-char-class :whitespace-char-class #\-)))
    "\""
    ,+inner-tag+
    ">"
    (:register (:non-greedy-repetition 0 nil
                (:branch (:negative-lookahead "<") :everything)))
    "</option>"))

(defun normalize-text (string)
  (let ((start (or (position-if-not #'cl-ppcre::whitespacep string)
                   0))
        (end (1+ (or (position-if-not #'cl-ppcre::whitespacep string :from-end t)
                     (1- (length string))))))
    (nsubstitute #\- #\ (subseq string start end))))

(defvar *ano-dados-comvest*)
(defvar *cursos-comvest*)
(defvar *cidades-comvest*)
(defvar *questoes-comvest*)
(defvar *grupos-comvest*)

(defun extrair-opcoes-comvest (ano)
  (unless (and (boundp '*ano-dados-comvest*)
               (= ano *ano-dados-comvest*))
    (setf *ano-dados-comvest* ano)
    (let ((opcoes '("opcao" "cid_inscricao" "questao" "grupo"))
          (variaveis '(*cursos-comvest* *cidades-comvest* *questoes-comvest*
                       *grupos-comvest*))
          (texto
           (http-request
            (format nil "http://www.comvest.unicamp.br/estatisticas/~d/quest/quest1.php"
                    ano))))
      (loop for opcao in opcoes
         for variavel in variaveis
         for subtexto = (scan-to-strings (regex-opcoes-comvest opcao) texto)
         do (setf (symbol-value variavel)
                  (loop for (start end register-start register-end)
                       = (multiple-value-list (scan *regex-opcoes-comvest-single*
                                                    subtexto :start (or end 0)))
                     while start collect
                       (cons (subseq subtexto (aref register-start 0)
                                     (aref register-end 0))
                             (normalize-text
                              (subseq subtexto (aref register-start 1)
                                      (aref register-end 1))))))))))

(defparameter *regex-comvest-single*
  `(:sequence
    "<td"
    ,+inner-tag+
    ">"
    (:register (:non-greedy-repetition
                0 nil
                (:alternation :word-char-class :whitespace-char-class
                              #\% #\. #\( #\) #\, #\- #\+ #\$ #\\ #\/ #\" #\')))
    "</td>"))

(defparameter *regex-comvest*
  '(:sequence
    "<span class=\"destaque\">"
    (:non-greedy-repetition
     0 nil (:alternation :everything :whitespace-char-class))
    "</span>"
    (:non-greedy-repetition
     0 nil (:alternation :everything :whitespace-char-class))
    "<span class=\"subtitulo\">"))

(defvar *dados-coletados*)
(defvar *respostas-questoes*)
(defvar *totais-por-grupo*)
(defconstant +maximo-questoes+ 20)
(defconstant +maximum-retries+ 10)
(defconstant +retry-time+ 0.5)
(defconstant +timeout-time+ 10)

(defun dados-comvest-single (ano curso cidade n-linha questao n-questao grupo n-grupo)
  (let* ((texto
          (scan-to-strings
           *regex-comvest*
           (loop for try from 0
              for response =
                (handler-case
                    (bt:with-timeout (+timeout-time+)
                      (http-request
                       (format
                        nil
                        "http://www.comvest.unicamp.br/estatisticas/~d/quest/quest2.php"
                        ano)
                       :method :post
                       :form-data t
                       :parameters `(("opcao" . ,curso)
                                     ("cid_inscricao" . ,cidade)
                                     ("questao" . ,questao)
                                     ("grupo" . ,grupo))))
                  (error (c)
                    (if (>= try +maximum-retries+)
                        (error c)
                        (sleep +retry-time+))))
              until response finally (return response))))
         (dados
          (loop for (start end register-start register-end)
               = (multiple-value-list (scan *regex-comvest-single*
                                            texto :start (or end 0)))
             while start collect
               (normalize-text
                (subseq texto (aref register-start 0)
                        (aref register-end 0))))))
    (loop while dados
       for n-resposta from 0
       for resposta = (pop dados)
       for numero = (parse-integer (pop dados))
       for porcentagem = (pop dados)
       ;; collect (list resposta numero porcentagem)
       do (assert (char= #\% (last-elt porcentagem)))
         (if (string= resposta "total")
             (if-let ((total (aref *totais-por-grupo* n-linha n-grupo)))
               (or (and (= (car total) numero)
                        (string= (cdr total) porcentagem))
                   (warn "Total de candidatos não coincide.~%~
Antes: ~A Depois: ~A~%~
Curso: ~A Grupo: ~A Cidade: ~A Ano: ~A"
                         total (cons numero porcentagem)
                         curso grupo cidade ano))
               (setf (aref *totais-por-grupo* n-linha n-grupo)
                     (cons numero porcentagem)))
             (progn
               (if-let ((respostas (aref *respostas-questoes* n-questao)))
                 (if-let ((posicao (position resposta respostas :test 'equal)))
                   (assert (= posicao n-resposta))
                   (progn
                     (assert (= (length respostas) n-resposta))
                     (setf (cdr (last respostas)) (list resposta))))
                 (progn
                   (assert (= 0 n-resposta))
                   (setf (aref *respostas-questoes* n-questao)
                         (list resposta))))
               (setf (aref *dados-coletados* n-linha n-questao n-grupo n-resposta)
                     (cons numero porcentagem)))))))

(defconstant +testing-boundary+ nil)

(declaim (inline maybe-clamp-list))

(defun maybe-clamp-list (list)
  #+sbcl (declare (optimize sb-ext:inhibit-warnings))
  (if +testing-boundary+
      (subseq list 0 +testing-boundary+)
      list))

(defun dados-comvest (ano &key (linhas :cursos) (tipo :porcentagem)
                      (stream *standard-output*) (report t) questoes)
  (extrair-opcoes-comvest ano)
  (let* ((*respostas-questoes* (make-array (length *questoes-comvest*)
                                           :initial-element nil))
         (*cursos-comvest* (if (eq linhas :cursos)
                               (maybe-clamp-list *cursos-comvest*)
                               (list (first *cursos-comvest*))))
         (*cidades-comvest* (if (eq linhas :cidades)
                                (maybe-clamp-list *cidades-comvest*)
                                (list (first *cidades-comvest*))))
         (*questoes-comvest* (if questoes
                                 (mapcar (lambda (questao)
                                           (assoc questao *questoes-comvest*
                                                  :test #'string=))
                                         questoes)
                                 (maybe-clamp-list *questoes-comvest*)))
         (*grupos-comvest* *grupos-comvest*)
         (n-linhas (ecase linhas
                     (:cursos (length *cursos-comvest*))
                     (:cidades (length *cidades-comvest*))))
         (*totais-por-grupo* (make-array (list n-linhas (length *grupos-comvest*))
                                         :initial-element ""))
         (*dados-coletados*
          (make-array (list n-linhas
                            (length *questoes-comvest*)
                            (length *grupos-comvest*)
                            +maximo-questoes+)
                      :initial-element (cons "" ""))))
    (when report
      (format *trace-output* "DADOS-COMVEST: Iniciando extração de dados Comvest (~A) ~%"
              ano))
    (loop for (curso) in *cursos-comvest*
       for n-curso from 0 do
         (loop for (cidade) in *cidades-comvest*
            for n-cidade from 0 do
              (when report
                (format *trace-output* "DADOS-COMVEST: ~A cursos/cidades de ~A concluídos ~%"
                        (+ n-curso n-cidade)
                        (+ (length *cursos-comvest*) (length *cidades-comvest*) -1)))
              (loop for (questao) in *questoes-comvest*
                 for n-questao from 0 do
                   (loop for (grupo) in *grupos-comvest*
                      for n-grupo from 0 do
                        (dados-comvest-single ano curso cidade
                                              (+ n-curso n-cidade)
                                              questao n-questao grupo n-grupo)))))
    (format *trace-output* "DADOS-COMVEST: Fim da extração.~%")
    (imprimir-tabela ano :linhas linhas :tipo tipo :stream stream)))

(defun imprimir-tabela (ano &key (linhas :cursos)
                        (tipo :porcentagem) (stream *standard-output*))
  (flet ((print-tabs (n)
           (dotimes (i n)
             (princ #\Tab stream)))
         (dado (celula &optional total-p)
           (format stream "~A~C"
                   (if total-p
                       (car celula)
                       (ecase tipo
                         (:porcentagem (substitute #\, #\. (cdr celula)))
                         (:numero (car celula))))
                   #\Tab)))
    ;; Cabeçalho
    (format stream "Dados Socioeconômicos Comvest Unicamp ~A~%~%" ano)
    ;; Linha 1
    (ecase linhas
      (:cursos  (format stream "Curso~CTotais" #\Tab))
      (:cidades (format stream "Cidade~CTotais" #\Tab)))
    (print-tabs (length *grupos-comvest*))
    (loop for (abrev . questao) in (maybe-clamp-list *questoes-comvest*)
       for n-questao from 0 do
       (format stream "~A" questao)
       (print-tabs (* (length (aref *respostas-questoes* n-questao))
                      (length *grupos-comvest*))))
    (terpri stream)
    ;; Linha 2
    (print-tabs 1)
    (print-tabs (length *grupos-comvest*))
    (loop for (abrev . questao) in (maybe-clamp-list *questoes-comvest*)
       for n-questao from 0 do
       (dolist (resposta (aref *respostas-questoes* n-questao))
         (format stream "~@(~A~)" resposta)
         (print-tabs (length *grupos-comvest*))))
    (terpri stream)
    ;; Linha 3
    (print-tabs 1)
    (flet ((print-grupos ()
             (loop for (abrev . grupo) in *grupos-comvest* do
                  (format stream "~A" (subseq grupo 0 2))
                  (print-tabs 1))))
      (print-grupos)
      (dotimes (n-questao (length (maybe-clamp-list *questoes-comvest*)))
        (dotimes (i (length (aref *respostas-questoes* n-questao)))
          (print-grupos))))
    (terpri stream)
    ;; Linhas de dados
    (loop for (abrev . curso) in (if (eq linhas :cursos)
                                     (maybe-clamp-list *cursos-comvest*)
                                     (list (first *cursos-comvest*)))
       for n-curso from 0 do
       (loop for (abrev . cidade) in (if (eq linhas :cidades)
                                         (maybe-clamp-list *cidades-comvest*)
                                         (list (first *cidades-comvest*)))
          for n-cidade from 0 do
          (ecase linhas
            (:cursos  (format stream "~A" curso))
            (:cidades (format stream "~A" cidade)))
          (print-tabs 1)
          (dotimes (n-grupo (length *grupos-comvest*))
            (dado (aref *totais-por-grupo* (+ n-curso n-cidade) n-grupo)
                  t))
          (dotimes (n-questao (length (maybe-clamp-list *questoes-comvest*)))
            (dotimes (n-resposta (length (aref *respostas-questoes* n-questao)))
              (dotimes (n-grupo (length *grupos-comvest*))
                (dado (aref *dados-coletados* (+ n-curso n-cidade)
                            n-questao n-grupo n-resposta)))))
          (terpri stream)))
    (terpri stream)
    (loop for (abrev . grupo) in *grupos-comvest* do
         (format stream "~A = ~A entre os ~A~%" (subseq grupo 0 2)
                 (ecase tipo
                   (:porcentagem "Porcentagem")
                   (:numero "Número total"))
                 (string-downcase grupo)))
    (terpri stream)
    (format stream "Fonte: Comvest (http://www.comvest.unicamp.br/)~%")))

(defun dados-comvest-arquivos (diretorio anos &key (linhas :cursos) (tipo :porcentagem)
                               questoes)
  (dolist (ano (ensure-list anos))
    (with-open-file (file (make-pathname :name
                                         (string-downcase
                                          (format nil "comvest ~a ~a ~a"
                                                  ano linhas tipo))
                                         :defaults diretorio)
                          :direction :output :if-exists :supersede)
      (dados-comvest ano :linhas linhas :tipo tipo :stream file :questoes questoes))))
