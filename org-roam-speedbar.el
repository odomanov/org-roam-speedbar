;;; org-roam-speedbar.el --- Speedbar display mode for Org-roam

;; Author: Oleg Domanov <odomanov@yandex.ru>
;; Version: 1.0
;; Keywords: org-roam speedbar

;;; Commentary:

;;;  First customize or setup the `org-roam-sb-startid'.  It should contain
;;;  the id for the root node.  Then launch `speedbar' as usual.
;;;  The command `o' switches to the Org-roam display mode.
;;;
;;;  Commands:
;;;    o           - Start from the very beginning.
;;;    RET, +, =   - Expand/contract the current node.
;;;    C-RET       - Open the file corresponding to the current node.
;;;    TAB         - Make the current node the root node and refresh the frame.
;;;    f, b        - Switch to the Files or Buffers display mode accordingly.


;;; Code:

(defcustom org-roam-sb-startid ""
  "The initial ID for Speedbar."
  :type 'string
  :group 'org-roam)

(require 'speedbar)

(defun org-roam-sb--expand (text token indent)
  "Expanding/contracting the item."
  (cond ((string-match "\\+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
         (let ((dests (org-roam-db-query
                       `[:select  [dest] :from links
                                  :where (= links:source $s1)] (caddr token))))
           (dolist (dest dests)
             (setq item (org-roam-db-query
                         `[:select  [file title id] :from nodes
                                    :where (= nodes:id $s1)] (car dest)))
	     (speedbar-with-writable
	       (save-excursion
	         (end-of-line) (forward-char)
                 (org-roam-sb--item-maybe-subitems item (1+ indent)))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun org-roam-sb--open (_ token _)
  "Open file with id extracted from TOKEN"
  ;; from org-roam
  (let ((node (org-roam-populate (org-roam-node-create :id (caddr token)))))
    (cond
      ((org-roam-node-file node)
       (org-mark-ring-push)
       (org-roam-node-visit node nil 'force)
       (speedbar-get-focus)
       t)
      (t nil))))

(defun org-roam-sb--item-maybe-subitems (item level)
  "Display ITEM with possible subitems on LEVEL.
ITEM is a list (file title id) possibly followed by a list
of those."
  (cond ((listp (car item))
         (dolist (i item) (org-roam-sb--item-maybe-subitems i (1+ level))))
        (t
         (speedbar-make-tag-line 'bracket ?+ #'org-roam-sb--expand item
                                 (cadr item) #'org-roam-sb--open item
                                 'org-link level))))

(defun org-roam-sb--start (startid)
  "Refresh display staring with STARTID."
  (speedbar-with-writable
    (erase-buffer)
    (insert "     =-=  Org-roam  =-=\n")
    (let ((start (org-roam-db-query
                  `[:select  [file title id] :from nodes
                             :where (= nodes:id $s1)] startid)))
      (org-roam-sb--item-maybe-subitems start 0)))
  (previous-line)
  (speedbar-expand-line))

(defun org-roam-sb--buttons (dir depth)
  "Buttons for DIR and DEPTH (not used actually)."
  (if (or (not org-roam-sb-startid)
          (equal org-roam-sb-startid ""))
      (insert "org-roam-sb-startid is not set")
    (progn
      (org-roam-sb--start org-roam-sb-startid)
      (speedbar-with-writable
        (visual-line-mode 0)))))

(defun org-roam-sb--TAB ()
  "Refresh display with the current item as root."
  (interactive)
  (end-of-line)
  (backward-char)
  (let ((start (caddr (get-text-property (point) 'speedbar-token))))
    (org-roam-sb--start start)))

(defun org-roam-sb--line-open ()
  "Open file at current position."
  (interactive)
  (save-excursion
    (end-of-line)
    (backward-char)
    (let ((tok (get-text-property (point) 'speedbar-token)))
      (org-roam-sb--open nil tok nil))))

(defun org-roam-sb--line-expand ()
  "Expand the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (when (looking-at "^\\([0-9]+\\):")
        (setq indent (string-to-number (match-string 1)))
        (when (re-search-forward "[]>?}] [^ ]" (line-end-position) t)
	  (backward-char 5)
          (let ((text (buffer-substring (point) (+ (point) 3)))
                (token (get-text-property (point) 'speedbar-token)))
            (if token
                (org-roam-sb--expand text token indent))))))))

(defun org-roam-sb--item-info ()
  "Info"
  (message "%s" (cadr (get-text-property (point) 'speedbar-token)))) 

;;;;;;;; Install function

(defun org-roam-sb--install ()
  "Install for Speedbar."
  (interactive)
  (setq org-roam-sb--menu-items '("")) 
  (setq org-roam-sb--mode-map (speedbar-make-specialized-keymap))
  (speedbar-add-expansion-list '("Org-roam"
                                 org-roam-sb--menu-items
                                 org-roam-sb--mode-map
                                 org-roam-sb--buttons))
  (let ((map org-roam-sb--mode-map))
    (define-key map (kbd "TAB") #'org-roam-sb--TAB)
    (define-key map (kbd "<return>") #'org-roam-sb--line-expand)
    (define-key map (kbd "<C-return>") #'org-roam-sb--line-open)
    (define-key map "+" #'org-roam-sb--line-expand)
    (define-key map "=" #'org-roam-sb--line-expand)
    (define-key map "o" (lambda () (interactive)
                          (speedbar-change-initial-expansion-list "Org-roam")))
    (define-key map "b" (lambda () (interactive)
                          (speedbar-change-initial-expansion-list "quick buffers")))
    (define-key map "f" (lambda () (interactive)
                          (speedbar-change-initial-expansion-list "files"))))
  (define-key speedbar-file-key-map    "o" (lambda () (interactive)
                                             (speedbar-change-initial-expansion-list "Org-roam")))
  (define-key speedbar-buffers-key-map "o" (lambda () (interactive)
                                             (speedbar-change-initial-expansion-list "Org-roam")))

  (speedbar-add-mode-functions-list
   '("Org-roam"
     (speedbar-item-info . org-roam-sb--item-info)
     ;;(speedbar-line-directory . MyExtension-speedbar-line-directory)
     )))

;; (with-eval-after-load 'speedbar
;;   (org-roam-sb--install))
(org-roam-sb--install)

(provide 'org-roam-speedbar)

;;; org-roam-speedbar.el ends here
