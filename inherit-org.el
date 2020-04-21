;;; inherit-org.el -- Inherit Org Faces to non-org buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/inherit-org
;; Keywords: faces
;; Created: 19 April 2020
;; Version: 1.0
;; Package-Requires: ((emacs "24") (org "9.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; inherit-org: Inherit Org Faces to non-org buffers
;; Inherit org faces to non-org buffers
;
;; The following major modes are supported:
;;
;; 1. w3m mode
;; 2. info mode
;; 3. helpful mode

;;; Code:

(require 'org-faces)
(require 'outline)
(require 'org-indent)


(defcustom inherit-org-bullets-bullet-list
  (or (bound-and-true-p org-bullets-bullet-list)
      (bound-and-true-p org-superstar-headline-bullets-list)
      '("◉"
        "○"
        "✸"
        "✿"))
  "Bullets for headings."
  :group 'inherit-org
  :type '(repeat (string :tag "Bullet character")))


(defvar inherit-org-outline-regexp (concat " ?+"
                                       (regexp-opt
                                        inherit-org-bullets-bullet-list
                                        t) " +")
  "TODO: Regexp to match inherit-org headlines.")

(defvar inherit-org-outline-regexp-bol (concat " ?+"
                                           (regexp-opt
                                            inherit-org-bullets-bullet-list
                                            t) "\\( +\\)")
  "TODO: Regexp to match inherit-org headlines.
This is similar to `inherit-org-outline-regexp' but additionally makes
sure that we are at the beginning of the line.")

(defvar inherit-org-imenu-regexp-bol (concat "^\\(?: ?+\\)"
                                         (regexp-opt
                                          inherit-org-bullets-bullet-list
                                          t) "\\( .*\\)$")
  "TODO: Regexp to match inherit-org headlines.
This is similar to `inherit-org-outline-regexp' but additionally makes
sure that we are at the beginning of the line.")

(defvar inherit-org-level 'inherit-org-level
  "Compute the header's nesting level in an outline.")

(defun inherit-org-w3m-headline-fontify ()
  "Fontify bold text in the buffer containing halfdump."
  (goto-char (point-min))
  (while (re-search-forward "^<b>.*</b>$" nil t)
    (beginning-of-line)
    (let ((start (match-beginning 0)))
      (insert (propertize (concat (car inherit-org-bullets-bullet-list) " ") 'face 'org-level-1))
      (delete-region start (match-beginning 0))
      (when (re-search-forward "</b>" nil t) ;; "</b[ \t\r\f\n]*"
        (delete-region (match-beginning 0) (match-end 0))
        (w3m-add-face-property start (match-beginning 0) 'org-level-1)))))

(defun inherit-org-helpful-heading (text)
  "Propertize TEXT as a heading."
  (format "%s\n" (propertize (concat "◉ " text) 'face 'org-level-2)))

(defun inherit-org-info-mode-fontify ()
  "Fontify info mode bufffer."
  ;; (face-remap-add-relative 'info-title-1 '(:height nil))
  ;; (face-remap-add-relative 'info-menu-header 'org-title)
  (face-remap-add-relative 'info-title-1 'org-level-1)
  (face-remap-add-relative 'info-title-2 'org-level-2)
  (face-remap-add-relative 'info-title-3 'org-level-3)
  (face-remap-add-relative 'info-title-4 'org-level-4)
  (face-remap-add-relative 'info-xref 'org-link)
  (face-remap-add-relative 'info-xref-visited 'org-done)
  (face-remap-add-relative 'Info-quoted 'org-verbatim))

(defun inherit-org-helpful-mode-fontify ()
  "Fontify helpful mode bufffer."
  ;; (face-remap-add-relative 'helpful-heading 'org-level-2)
  (advice-add 'helpful--heading :override 'inherit-org-helpful-heading))

(defun inherit-org-w3m-mode-fontify ()
  "Fontify w3m mode bufffer."
  ;; (face-remap-add-relative 'w3m-header-line-title 'org-level-1)
  (face-remap-add-relative 'w3m-anchor 'org-link)
  ;; (face-remap-add-relative 'w3m-current-anchor 'link)
  )

(defun inherit-org-js2-mode-fontify ()
  "Fontify info mode bufffer."
  (interactive)
  (font-lock-add-keywords nil '(("◉.*" (0 'org-level-1 t))) 'prepend)
  (font-lock-add-keywords nil '(("○.*" (0 'org-level-2 t))) 'prepend)
  (font-lock-add-keywords nil '(("✸.*" (0 'org-level-3 t))) 'prepend)
  (font-lock-add-keywords nil '(("✿.*" (0 'org-level-4 t))) 'prepend)
  (font-lock-add-keywords nil '(("\\*\\*\\*\\* .*" (0 'org-level-4 t))) 'prepend))

(defun inherit-org ()
  "Fontify all supported major mode."
  (interactive)
  (cond ((eq major-mode 'w3m-mode)
         (inherit-org-w3m-mode-fontify)
         (inherit-org-additional-features-on))
        ((eq major-mode 'Info-mode)
         (inherit-org-info-mode-fontify))
        ((eq major-mode 'helpful-mode)
         (inherit-org-helpful-mode-fontify)
         (inherit-org-additional-features-on))))

(defun inherit-org-additional-features-off ()
  (setq imenu-create-index-function nil)
  (outline-minor-mode -1)
  (org-indent-mode -1))

(defun inherit-org-additional-features-on ()
  (inherit-org-regexp)
  (setq imenu-create-index-function #'inherit-org-imenu-get-tree)
  (outline-minor-mode)
  (org-indent-mode))

;;;###autoload
(defun inherit-org-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (org-with-wide-buffer
   (goto-char (point-max))
   (let* ((re inherit-org-imenu-regexp-bol)
          (subs (make-vector (1+ org-imenu-depth) nil))
          (last-level 0))
     (while (re-search-backward re nil t)
       ;; (message (int-to-string (inherit-org-level (match-string 1))))
       (let ((level (1- (funcall inherit-org-level)))
             (headline (match-string 2)))
         (message (int-to-string level ))
         (message headline)
         ;; (when  (<= level org-imenu-depth)
         (when (and (<= level org-imenu-depth) (org-string-nw-p headline))
           (let* ((m (point-marker))
                  (item (propertize headline 'org-imenu-marker m 'org-imenu t)))
             (message item)
             (push m org-imenu-markers)
             (if (>= level last-level)
                 (push (cons item m) (aref subs level))
               (push (cons item
                           (cl-mapcan #'identity (cl-subseq subs (1+ level))))
                     (aref subs level))
               (cl-loop for i from (1+ level) to org-imenu-depth
                        do (aset subs i nil)))
             (setq last-level level)))))
     (aref subs 0))))

(defun inherit-org-level ()
  "Function of no args to compute a header's nesting level in an outline."
  (1+ (cl-position (match-string 1) inherit-org-bullets-bullet-list :test 'equal)))

(defun inherit-org-regexp ()
  "Set regexp for outline minior mode."
  (setq-local outline-regexp inherit-org-outline-regexp)
  (setq-local org-outline-regexp-bol outline-regexp) ; for org-cycle, org-shifttab
  (setq-local org-outline-regexp outline-regexp) ; for org-cycle, org-shifttab
  (setq-local org-complex-heading-regexp outline-regexp) ; for org-cycle, org-shifttab
  (setq-local outline-level inherit-org-level))


;;;###autoload
(define-minor-mode inherit-org-mode
  "Toggle shr minor mode.
1. imenu
2. outline-minor-mode
3. org-indent-mode "
  :group 'inherit-org
  (cond
   (inherit-org-mode
    (inherit-org))
   (t
    (inherit-org-additional-features-off))))

;; (add-hook 'inherit-org-mode-hook #'inherit-org)









(defgroup lisp-extra-font-lock nil
  "Highlight bound variables and quoted expressions in lisp."
  :group 'faces)


;;;###autoload
(defcustom lisp-extra-font-lock-modes '(emacs-lisp-mode lisp-mode)
  "List of modes where Lisp Extra Font Lock Global mode should be enabled."
  :type '(repeat symbol)
  :group 'lisp-extra-font-lock)


;; ----------
;; Faces and corresponding variable.
;;

(defface lisp-extra-font-lock-backquote
  '((t :inherit font-lock-warning-face))
  "The default face used to highlight backquotes and the comma operator."
  :group 'org-level-1)

(defcustom lisp-extra-font-lock-backquote-face 'lisp-extra-font-lock-backquote
  "The face used to highlight backquotes and the comma operator.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


(defface lisp-extra-font-lock-quoted
  '((t :inherit font-lock-constant-face))
  "The default face used to highlight quoted expressions."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-quoted-face 'lisp-extra-font-lock-quoted
  "The face used to highlight quoted expressions.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


(defface lisp-extra-font-lock-quoted-function
  '((t :inherit font-lock-function-name-face))
  "The default face used to highlight #'-quoted function symbols."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-quoted-function-face
  'lisp-extra-font-lock-quoted-function
  "The face used to highlight #'-quoted function symbols.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


(defface lisp-extra-font-lock-special-variable-name
  '((t :inherit org-level-1))
  "The default face used to highlight special variables bound by `let'."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-special-variable-name-face
  'lisp-extra-font-lock-special-variable-name
  "The face used to highlight special variables bound by `let'.

A special variable is a global variable defined by `defvar'. See
`special-variable-p' for details.

To disable this highlighting, set this to nil. To highlight
special variables like plain variables, set this to
`font-lock-variable-name-face'."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


;; ----------
;; Function lists
;;

(defcustom lisp-extra-font-lock-let-functions
  '("let"
    "let*"
    "letf"
    "letf*"
    "lexical-let"
    "lexical-let*"
    "multiple-value-bind"
    "pcase-let"                         ; Highlights entire UPAT:s.
    "pcase-let*"
    "cl-letf"
    "cl-letf*"
    "cl-multiple-value-bind")
  "List of function using same syntax as `let' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-defun-functions
  '("defun"
    "defun*"
    "defmacro"
    "defmacro*"
    "defsubst"
    "cl-defun"
    "cl-defmacro"
    "cl-defsubst")
  "List of function using same syntax as `defun' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-lambda-functions
  '("lambda")
  "List of function using same syntax as `lambda' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-dolist-functions
  '("dolist"
    "dotimes"
    "cl-dolist"
    "cl-dotimes")
  "List of function using same syntax as `dolist' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-bind-first-functions
  '("condition-case")
  "List of function that bind their first argument."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-loop-functions
  '("loop"
    "cl-loop")
  "List of functions using same syntax as `loop' to bind variables.."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


;; ------------------------------
;; The modes
;;

;;;###autoload
(define-minor-mode lisp-extra-font-lock-mode
  "Minor mode that highlights bound variables and quoted expressions in lisp."
  :group 'lisp-extra-font-lock
  (if lisp-extra-font-lock-mode
      (lisp-extra-font-lock-add-keywords)
    (lisp-extra-font-lock-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(define-global-minor-mode lisp-extra-font-lock-global-mode
  lisp-extra-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p lisp-extra-font-lock-modes)
      (lisp-extra-font-lock-mode 1)))
  :group 'lisp-extra-font-lock)


(defun lisp-extra-font-lock-variable-face-form (name)
  "A form suitable for a font-lock face expression.

NAME is a form that should evalute to the name of the symbol, as a string."
  `(if (ignore-errors (let ((symbol (intern-soft ,name)))
                        (and symbol
                             (special-variable-p symbol))))
       lisp-extra-font-lock-special-variable-name-face
     font-lock-variable-name-face))

(defun lisp-extra-font-lock-keywords ()
  "Font-lock keywords used by `lisp-extra-font-lock'.
The keywords highlight variable bindings and quoted expressions."
  `(;; Function and lambda parameters
    (,(concat "("
              "\\(?:"
              (regexp-opt lisp-extra-font-lock-defun-functions)
              "[ \t\n]+\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
              "\\|"
              (regexp-opt lisp-extra-font-lock-lambda-functions)
              "\\)"
              "[ \t\n]+(")
     (lisp-extra-font-lock-match-argument-list
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (save-excursion
          (backward-char)               ; Position point before "(".
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form
      nil
      (0 ,(lisp-extra-font-lock-variable-face-form '(match-string 0))
         nil t)))
    ;; Variables bound by `let'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-let-functions)
              "[ \t]+(")
     (lisp-extra-font-lock-match-let
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (save-excursion
          (backward-char)               ; Position point before "(".
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form
      (goto-char (match-end 0))
      (0 ,(lisp-extra-font-lock-variable-face-form '(match-string 0)))))
    ;; Variables bound by `cl-dolist' etc.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-dolist-functions)
              "[ \t]+(\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))))
    ;; Bind first argument like `condition-case'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-bind-first-functions)
              "[ \t]+\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 (and (not (string= (match-string 1) "nil"))
             ,(lisp-extra-font-lock-variable-face-form '(match-string 1)))))
    ;; Bind variables and named arguments to `cl-loop'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-loop-functions)
              "\\_>")
     (lisp-extra-font-lock-match-loop-keywords
      ;; Pre-match form. Value of expression is limit for submatcher.
      (progn
        (goto-char (match-end 0))
        (save-excursion
          (goto-char (match-beginning 0))
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form.
      (goto-char (match-end 0))
      (1 font-lock-builtin-face)
      (2 ,(lisp-extra-font-lock-variable-face-form '(match-string 2)) nil t)))
    (;; Quote and backquote.
     ;;
     ;; Matcher: Set match-data 1 if backquote.
     lisp-extra-font-lock-match-quote-and-backquote
     (1 lisp-extra-font-lock-backquote-face nil t)
     (;; Submatcher, match part of quoted expression or comma.
      lisp-extra-font-lock-match-quoted-content
      ;; Pre-match form. Value of expression is limit for submatcher.
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (lisp-extra-font-lock-end-position))
      ;; Post-match form
      (goto-char (match-end 0))
      ;; Highlight rules for submatcher.
      (1 lisp-extra-font-lock-quoted-face append)
      (2 lisp-extra-font-lock-backquote-face nil t)))
    ;; Function read syntax
    ("#'\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>"
     1 lisp-extra-font-lock-quoted-function-face)))


(defvar lisp-extra-font-lock--installed-keywords nil)

(defun lisp-extra-font-lock-add-keywords ()
  "Add extra font-lock keywords to lisp."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'lisp-extra-font-lock--installed-keywords)
    (font-lock-remove-keywords nil lisp-extra-font-lock--installed-keywords))
  (let ((keywords (lisp-extra-font-lock-keywords)))
    (set (make-local-variable 'lisp-extra-font-lock--installed-keywords)
         keywords)
    (font-lock-add-keywords nil keywords 'append)))


(defun lisp-extra-font-lock-remove-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil lisp-extra-font-lock--installed-keywords))


;; ----------------------------------------
;; Matcher functions
;;

(defun lisp-extra-font-lock-end-position ()
  "Suitable end position of expression after point.
If expression is open-ended, the beginning of the next top-level
form is used, or `point-max' if none is found."
  (save-match-data
    (save-excursion
      (or (condition-case nil
              (progn
                (forward-sexp)
                (point))
            (error nil))
          (and (re-search-forward "^(" nil t)
               (match-beginning 0))
          (point-max)))))

(defun lisp-extra-font-lock-match-argument-list (limit)
  (forward-comment (buffer-size))
  (and (< (point) limit)
       (let ((res (looking-at "\\_<\\(?:\\sw\\|\\s_\\)+\\_>")))
         (when res
           (goto-char (match-end 0)))
         res)))


(defun lisp-extra-font-lock-match-let (limit)
  "Match next variable introduced by `let'-like constructs."
  (forward-comment (buffer-size))
  (let ((p (point)))
    (cond ((eq (following-char) ?\( )
           ;; Match "(var initial-valoue)"
           (forward-char)
           (forward-comment (buffer-size))
           (and
            (< (point) limit)
            (let ((res (looking-at "\\(?:\\sw\\|\\s_\\)+\\_>")))
              (when res
                (goto-char p)
                (condition-case nil
                    (forward-sexp)
                  (error (goto-char limit))))
              res)))
          ((looking-at "\\(?:\\sw\\|\\s_\\)+\\_>")
           ;; Match "var"
           (goto-char (match-end 0))
           (<= (point) limit))
          (t
           nil))))


(defun lisp-extra-font-lock-is-in-comment-or-string (pos)
  "Return non-nil if POS is in a comment, string, constant, or reader macro.

This assumes that Font Lock is active and has fontified comments
and strings."
  (or (nth 8 (save-excursion
	       (syntax-ppss pos)))   ; In comment or string.
      ;; Plain character constant ?<char>.
      (eq (char-before pos) ??)
      ;; Escaped character constant ?\<char>.
      (and (eq (char-before pos) ?\\)
           (eq (char-before (- pos 1)) ??))
      ;; Reader macro like #'.
      (eq (char-before pos) ?#)))


(defun lisp-extra-font-lock-match-quote-and-backquote (limit)
  "Search for quote and backquote in in code.
Set match data 1 if character matched is backquote."
  (let (res)
    (while
        (progn (setq res (re-search-forward "\\(?:\\(`\\)\\|'\\)" limit t))
               (and res
                    (lisp-extra-font-lock-is-in-comment-or-string
                     (match-beginning 0)))))
    res))


(defun lisp-extra-font-lock-match-quoted-content (limit)
  "Match next part of a quoted content.

Match up to next comma operator or quoted subexpression, or to
the end of the quoted expression."
  (and (< (point) limit)
       (let ((p (point))
             res)
         (while
             (progn
               (setq res (re-search-forward "\\(,@?\\|[`']\\)" limit t))
               (and res
                    (lisp-extra-font-lock-is-in-comment-or-string
                     (match-beginning 0)))))
         (if res
             ;; Match up to next quoted subpart or comma operator.
             (let ((is-comma (eq (char-after (match-beginning 0)) ?,)))
               (set-match-data (list
                                ;; Match data 0: Full match.
                                p (match-end 0)
                                ;; Match data 1: Part of the quoted expression
                                p
                                (match-beginning 0)
                                ;; Match data 2; Comma operator (if present)
                                (and is-comma (match-beginning 0))
                                (and is-comma (match-end 0))))
               (condition-case nil
                   (forward-sexp)
                 (error (goto-char limit))))
           ;; Match to the end of the quoted expression.
           (set-match-data (list p limit
                                 p limit))
           (goto-char limit))
         t)))

(defvar lisp-extra-font-lock-loop-keywords
  '("=" "above" "across" "across-ref" "always" "and" "append" "as"
    "being" "below" "buffer" "buffers" "by"
    "collect" "collecting" "concat" "count"
    "do" "doing" "downfrom" "downto"
    "each" "element" "elements" "else" "end"
    "extent" "extents" "external-symbol" "external-symbols"
    "finally" "frames" "from"
    "hash-key" "hash-keys" "hash-value" "hash-values"
    "if" "in" "in-ref" "initially" "interval" "intervals"
    "key-binding" "key-bindings" "key-code" "key-codes" "key-seq" "key-seqs"
    "maximize" "minimize"
    "named" "nconc" "nconcing" "never"
    "of" "of-ref" "on" "overlay" "overlays"
    "present-symbol" "present-symbols" "property"
    "repeat" "return"
    "screen" "screens" "sum" "symbol" "symbols"
    "the" "then" "thereis" "to"
    "unless" "until" "upfrom" "upto" "using"
    "vconcat"
    "when" "while" "windows")
  "List of `cl-loop' named parameters, excluding variable binding ones.")

(defvar lisp-extra-font-lock-loop-keywords-with-var '("for"
                                                      "index"
                                                      "into"
                                                      "with")
  "List of `cl-loop' named variable binding parameters.")


;; Match named loop keywords, and (optionally) any bound variables.
;;
;; Note, does not support "destructuring", i.e. binding several
;; variables using pattern matching. If this is used, the entire
;; expression is highlighted as a variable.
(defun lisp-extra-font-lock-match-loop-keywords (limit)
  "Match named keyword of `loop' and highlight variable arguments."
  (while
      (progn
        (forward-comment (buffer-size))
        (and (< (point) limit)
             (not (looking-at
                   (concat
                    "\\_<"
                    "\\("
                    (regexp-opt (append
                                 lisp-extra-font-lock-loop-keywords-with-var
                                 lisp-extra-font-lock-loop-keywords))
                    "\\)"
                    "\\_>")))))
    (condition-case nil
        (forward-sexp)
      (error (goto-char limit))))
  (if (not (< (point) limit))
      nil
    (goto-char (match-end 0))
    (when (member (match-string 1) lisp-extra-font-lock-loop-keywords-with-var)
      (forward-comment (buffer-size))
      (let ((var-start (point)))
        (when (condition-case nil
                  (progn
                    (forward-sexp)
                    t)
                (error nil))
          (set-match-data (list
                           (match-beginning 0)
                           (point)
                           (match-beginning 1)
                           (match-end 1)
                           var-start
                           (point))))))
    t))




(provide 'inherit-org)

;;; inherit-org.el ends here
