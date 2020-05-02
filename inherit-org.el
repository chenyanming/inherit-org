;;; inherit-org.el --- Inherit Org Faces to non-org buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/inherit-org
;; Keywords: faces
;; Created: 19 April 2020
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (org "9.0"))

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

(defun inherit-org-fontify-buffer ()
  "Fontify any bufffers just like org mode.
For Programming modes, after comment start, insert */+/- follow by a space.
For Text modes, fundemental mode, you can just insert */+/- as start just like org mode."
  (interactive)
  (font-lock-mode)
  (let ((h1 (concat comment-start ".*\\* .*"))
        (h2 (concat comment-start ".*\\*\\* .*"))
        (h3 (concat comment-start ".*\\*\\*\\* .*"))
        (h4 (concat comment-start ".*\\*\\*\\*\\* .*"))
        (h5 (concat comment-start ".*\\*\\*\\*\\*\\* .*"))
        (h6 (concat comment-start ".*\\*\\*\\*\\*\\*\\* .*"))
        (h7 (concat comment-start ".*\\*\\*\\*\\*\\*\\*\\* .*"))
        (h8 (concat comment-start ".*\\*\\*\\*\\*\\*\\*\\*\\* .*"))
        (plus (concat comment-start ".*\\+ .*"))
        (minus (concat comment-start ".*\\- .*")))
    (font-lock-add-keywords nil `((,h1 (0 'org-level-1 t))) 'prepend)
    (font-lock-add-keywords nil `((,h2 (0 'org-level-2 t))) 'prepend)
    (font-lock-add-keywords nil `((,h3 (0 'org-level-3 t))) 'prepend)
    (font-lock-add-keywords nil `((,h4 (0 'org-level-4 t))) 'prepend)
    (font-lock-add-keywords nil `((,h5 (0 'org-level-5 t))) 'prepend)
    (font-lock-add-keywords nil `((,h6 (0 'org-level-6 t))) 'prepend)
    (font-lock-add-keywords nil `((,h7 (0 'org-level-7 t))) 'prepend)
    (font-lock-add-keywords nil `((,h8 (0 'org-level-8 t))) 'prepend)
    (font-lock-add-keywords nil `((,plus (0 'org-list-dt t))) 'prepend)
    (font-lock-add-keywords nil `((,minus (0 'org-list-dt t))) 'prepend)))

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
   ;; (font-lock-remove-keywords nil inherit-org--font-lock-keywords)
    (inherit-org-additional-features-off))))

;; (add-hook 'inherit-org-mode-hook #'inherit-org)
(provide 'inherit-org)

;;; inherit-org.el ends here
