;;; head-context-sticky.el --- Sticky Scroll (Global Registry & Anti-Flicker) -*- lexical-binding: t; -*-

;; Author: Jahcw
;; Version: 1.0
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'seq)
(require 'subr-x)
(require 'color)

(defgroup head-context-sticky nil
  "Context sticky header settings."
  :group 'tools)

;; -----------------------------------------------------------------------------
;; 1. Configuration
;; -----------------------------------------------------------------------------
(defcustom head-context-sticky-invisible nil "Make sticky frame invisible")

(defcustom head-context-sticky-max-lines 5
  "Maximum number of lines to display."
  :type 'integer)

(defcustom head-context-sticky-show-line-numbers t
  "Whether to show line numbers in sticky header."
  :type 'boolean)

(defcustom head-context-sticky-excluded-modes 
  '(fundamental-mode  minibuffer-mode   dired-mode    magit-mode  shell-mode
                      compilation-mode  special-mode      messages-buffer-mode
                      ediff-mode Buffer-menu-mode help-mode debugger-mode)
  "Modes to disable sticky scroll."
  :type '(repeat symbol))

(defcustom head-context-sticky-node-rules
  '(;; Specific Overrides
    (python-mode     . ("function_definition" "class_definition" "if_statement" "for_statement" "while_statement" "try_statement" "with_statement" "match_statement"))
    (python-ts-mode  . ("function_definition" "class_definition" "if_statement" "for_statement" "while_statement" "try_statement" "with_statement" "match_statement"))
    (emacs-lisp-mode . ("defun" "cl-defun" "cl-defmethod" "defmacro"))
    (yaml-ts-mode    . ("block_mapping_pair"))
    (json-ts-mode    . ("pair"))
    (toml-ts-mode    . ("table" "pair"))

    ;; --- DEFAULT GENERIC RULE ---
    (default . (;; Functions/Methods
                "function_definition" "function_declaration" "function_item" "method_definition" "method_declaration" "arrow_function" 
                ;; Classes/Structs
                "class_definition" "class_declaration" "class_specifier" "struct_specifier" "interface_declaration" "impl_item" "trait_item"
                ;; Modules/Namespaces
                "namespace_definition" "module" "mod_item" "package_clause"
                ;; Control Flow (Statement style)
                "if_statement" "for_statement" "while_statement" "switch_statement" "do_statement" "try_statement" "case_statement"
                ;; Control Flow (Expression style)
                "if_expression" "for_expression" "while_expression" "match_expression" "loop_expression"
                ;; Misc
                "section" "headline" "tag" "element")))
  "Tree-sitter node types to display."
  :type '(alist :key-type symbol :value-type (repeat string)))

;; Faces
(defface head-context-sticky-number-face '((t :inherit line-number)) "Face for line numbers.")

;; -----------------------------------------------------------------------------
;; 2. Core Logic: Data Retrieval (Tree-sitter & Fallback)
;; -----------------------------------------------------------------------------

(defun head-context-sticky--get-context-data (window)
  (with-selected-window window
    (save-restriction
      (widen)
      (let* ((win-start (window-start))
             (pos (save-excursion
                    (goto-char win-start)
                    (forward-line head-context-sticky-max-lines)
                    (point)))
             (curr-line (line-number-at-pos pos))
             (top-line (- curr-line head-context-sticky-max-lines))
             (rules (or (alist-get major-mode head-context-sticky-node-rules)
                        (alist-get 'default head-context-sticky-node-rules)))
             res)
        (or 
         ;; 1. Strategy: Tree-sitter
         (when (and rules (featurep 'treesit) (treesit-parser-list))
           (condition-case nil
               (let ((node (treesit-node-at pos))
                     (seen-lines (make-hash-table :test 'eql)))
                 (while node
                   (let ((type (treesit-node-type node))
                         (start (treesit-node-start node)))                    
                     (when (and (< start pos) (member type rules))
                       (let ((line (- curr-line (count-lines start pos))))
                         (unless (gethash line seen-lines)
                           (puthash line t seen-lines)
                           (push (list :start start :line line) res)))))
                   (setq node (treesit-node-parent node)))
                 res)
             (error nil)))

         ;; 2. Strategy: Indentation
         (save-excursion
           (goto-char pos)
           (let ((curr (current-indentation)) (limit 150))
             (while (and (> (point) (point-min)) (> limit 0) (> curr 0))
               (forward-line -1) (cl-decf limit)
               (let ((ind (current-indentation))
                     (line-str (thing-at-point 'line t)))
                 (when (and (< ind curr) (not (string-blank-p line-str)))
                   (push (list :start (point)
                               :line (- curr-line (count-lines (point) pos))) res)
                   (setq curr ind))))
             res))

         ;; 3. Strategy: Outline
         (let ((count 0))
           (save-excursion
             (goto-char pos)
             ;; 1. 先移动到当前位置所属的标题行（如果当前就在标题上，则不动）
             (ignore-errors (outline-back-to-heading t))
             
             ;; 2. 循环向上寻找父级标题
             (while (and (< count head-context-sticky-max-lines)
                         (ignore-errors (outline-up-heading 1 t)))
               (let ((start (point)))
                 (push (list :start start
                             :line (- curr-line (count-lines start pos))) res)
                 (cl-incf count)))
             res)))

        (let* ((ln (length res))
               (n (if (> ln head-context-sticky-max-lines)
                      head-context-sticky-max-lines
                    (or ln 0))))          
          ;; Filter empty and sort
          (seq-filter 
           (lambda (item)
             (let ((s (plist-get item :start))
                   (l (plist-get item :line)))
               (save-excursion 
                 (goto-char s)
                 (and (< (+ 2 l) (+ top-line n))
                      (not (string-blank-p (buffer-substring s (line-end-position))))))))
           res))))))

;; -----------------------------------------------------------------------------
;; 3. Interaction & Registry
;; -----------------------------------------------------------------------------

(defvar head-context-sticky-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'head-context-sticky--jump)
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [wheel-up] #'head-context-sticky--scroll-parent)
    (define-key map [wheel-down] #'head-context-sticky--scroll-parent)
    (define-key map [double-wheel-up] #'ignore)
    (define-key map [double-wheel-down] #'ignore)
    map))

(defun head-context-sticky--scroll-parent (event)
  "将滚轮事件转发给父窗口。"
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (window-frame window))
         (parent-win (frame-parameter frame 'head-sticky-parent)))
    (when (window-live-p parent-win)
      (with-selected-window parent-win
        (let ((command (event-basic-type event)))
          (cond ((eq command 'wheel-up) (scroll-down 3))
                ((eq command 'wheel-down) (scroll-up 3))))))))

(defun head-context-sticky--jump (event)
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (point (posn-point posn)) ; 获取点击处的 Buffer 位置
         (line (when (and window point)
                 (with-current-buffer (window-buffer window)
                   (get-text-property point 'src-line-number)))))
    (when line
      (let* ((child-frame (window-frame window))
             (parent-window (frame-parameter child-frame 'head-sticky-parent)))
        (when (window-live-p parent-window)
          (select-window parent-window)
          (goto-line line)
          (recenter 5)
          (pulse-momentary-highlight-one-line (point)))))))

;; -----------------------------------------------------------------------------
;; 4. Frame Management (Global Registry - Fixes Split Window)
;; -----------------------------------------------------------------------------

;; Use a weak hash table to map Window Objects -> Frame Objects.
;; Weak keys mean if the window is deleted, the entry is eventually GC'd.
(defvar head-context-sticky--registry (make-hash-table :test 'eq :weakness 'key))
(defun head-context-sticky--get-frame (window)
  "Get sticky frame handle acorrding to WINDOW."
  (let ((stick (gethash window head-context-sticky--registry)))
    (if (frame-live-p (car stick))
        stick
      (remhash window head-context-sticky--registry) nil)))

;; -----------------------------------------------------------------------------
;; 2. 渲染优化：全局 Buffer 复用
;; -----------------------------------------------------------------------------

(defun head-context-sticky--get-or-create-buffer (window)
  "为特定窗口获取或创建一个唯一的渲染 Buffer。"
  (let ((sbuf (format " *head-sticky-render-%x*" (sxhash window))))
    (or (get-buffer sbuf)
        (let ((buf (get-buffer-create sbuf)))
          (with-current-buffer buf
            (setq mode-line-format nil 
                  header-line-format nil
                  display-line-numbers nil)
            buf)))))

(defun head-context-sticky--cleanup-window (window)
  "Destroy frame and sticky-buffer related to WINDOW"
  (let* ((stick (head-context-sticky--get-frame window))
         (frame (car stick))
         (sbuf (cdr stick)))
    (set-window-parameter window 'head-sticky-last-content nil)
    (when (frame-live-p frame) (delete-frame frame))
    (when (get-buffer (or sbuf "")) (kill-buffer sbuf))
    (remhash window head-context-sticky--registry)))

(defun head-context-sticky--gc (&optional _)
  "Cleanup dead frames and invalid registry entries."
  (maphash (lambda (win s)
             (let ((frame (car s)) (sbuf (cdr s)))
               (unless
                   (and (window-live-p win)
                        (frame-live-p frame)
                        (frame-visible-p (window-frame win)))
                 (when (frame-live-p frame) (delete-frame frame))
                 (when (get-buffer sbuf) (kill-buffer sbuf))
                 (remhash win head-context-sticky--registry))))
           head-context-sticky--registry))

;; -----------------------------------------------------------------------------
;; 5. Rendering Engine (Anti-Flicker & Strict Geometry)
;; -----------------------------------------------------------------------------

(defvar head-context-sticky--bg-cache nil)
(defun head-context-sticky--get-bg-color ()
  (or head-context-sticky--bg-cache
      (let* ((def-bg (face-attribute 'default :background))
             (valid-bg (if (and (stringp def-bg) (not (string= def-bg "unspecified"))) def-bg "#333333"))
             (rgb (color-name-to-rgb valid-bg))
             (is-dark (< (apply #'+ rgb) 1.5))
             (adj-percent (if is-dark 0.05 -0.05)))
        (setq head-context-sticky--bg-cache
              (condition-case nil
                  (let* ((hsl (apply #'color-rgb-to-hsl rgb))
                         (h (nth 0 hsl)) (s (nth 1 hsl)) (l (nth 2 hsl))
                         (new-l (max 0.0 (min 1.0 (+ l adj-percent)))))
                    (apply #'color-rgb-to-hex (color-hsl-to-rgb h s new-l)))
                (error valid-bg))))))

(defun head-context-sticky--reset-bg-cache (&rest _)
  (setq head-context-sticky--bg-cache nil))
(add-hook 'enable-theme-functions #'head-context-sticky--reset-bg-cache)

(defun head-context-sticky--update-window (window)
  (when (and (display-graphic-p)
             (window-live-p window)
             (not (window-minibuffer-p window))
             (frame-visible-p (window-frame window)))
    
    (let* ((buffer (window-buffer window))
           (stick (head-context-sticky--get-frame window))
           (frame (car stick))
           (sbuf (or (cdr stick) (head-context-sticky--get-or-create-buffer window))))
      
      (if (or (not (buffer-local-value 'head-context-sticky-mode buffer))
              (memq (buffer-local-value 'major-mode buffer) head-context-sticky-excluded-modes)
              head-context-sticky-invisible)
          (and (frame-live-p frame) (make-frame-invisible frame))    
        (let* ((data (seq-take (head-context-sticky--get-context-data window)
                               head-context-sticky-max-lines))
               (new-fp (mapcar (lambda (item) (plist-get item :start)) data))
               (old-fp (window-parameter window 'head-sticky-last-content)))

          ;; 1. 确保 Frame 存在
          (unless frame
            (let ((bg (head-context-sticky--get-bg-color)))
              (setq frame (make-frame `((parent-frame . ,(window-frame window))
                                        (minibuffer . nil)
                                        (undecorated . t)
                                        (internal-border-width . 0)
                                        (height . 0)
                                        (no-accept-focus . nil)
                                        (no-focus-on-map . t)
                                        (visibility . nil)
                                        (cursor-type . nil)
                                        (drag-internal-border . nil)
                                        (menu-bar-lines . 0)
                                        (tool-bar-lines . 0)
                                        (tab-bar-lines . 0)
                                        (border-width . 1)
                                        (left-fringe . 0) (right-fringe . 0)
                                        (vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)
                                        (background-color . ,bg)
                                        (head-sticky-parent . ,window))))
              (puthash window (cons frame sbuf) head-context-sticky--registry)))

          ;; 2. 计算几何位置
          (let* ((edges (window-inside-pixel-edges window))
                 (tab-h (window-tab-line-height window))
                 (head-h (window-header-line-height window))
                 (text-x (nth 0 edges)) 
                 (text-y (+ (nth 1 edges) tab-h head-h))
                 (text-w (- (nth 2 edges) text-x))
                 (line-h (frame-char-height frame))
                 (lnc (length data))
                 (target-h (* lnc line-h))
                 (window-min-height 0))
            ;; 3. 指纹对比，决定是否渲染
            (unless (equal new-fp old-fp)
              (set-window-parameter window 'head-sticky-last-content new-fp)
              (with-current-buffer sbuf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (head-context-sticky--render-to-buffer buffer data text-w)
                  ;; 这里直接复用 buffer，避免重复 set-window-buffer
                  (let ((root (frame-root-window frame)))
                    (unless (eq (window-buffer root) sbuf)
                      (set-window-buffer root sbuf))
                    (set-window-fringes root 0 0))))
              (set-frame-size frame text-w target-h t))


            ;; 4. 同步坐标并显示 (防闪烁：仅在坐标变化时移动)
            (let ((curr-x (frame-parameter frame 'left))
                  (curr-y (frame-parameter frame 'top)))
              (unless (and (eql curr-x text-x) (eql curr-y text-y))
                (set-frame-position frame text-x text-y)))
            
            (if (= 0 lnc)
                (make-frame-invisible frame)
              (or (frame-visible-p frame) (make-frame-visible frame))
              (raise-frame frame))))))))

;; -----------------------------------------------------------------------------
;; 4. 性能优化点总结
;; -----------------------------------------------------------------------------

;; A. 搜索优化：限制 Tree-sitter 向上回溯的层数 (防止在大文件中假死)
;; 在 head-context-sticky--get-context-data 中增加计数限制：
;; (while (and node (< count 20)) ...) 

;; B. 渲染优化：利用 font-lock-ensure 的范围限制
(defun head-context-sticky--render-to-buffer (src-buf data text-w)
  "辅助函数：将上下文数据渲染到目标 Buffer。"
  (let* ((char-w (frame-char-width))
         (active-ln (buffer-local-value 'display-line-numbers-mode src-buf))
         (ln-width (if (and head-context-sticky-show-line-numbers active-ln)
                       (with-current-buffer src-buf (line-number-display-width)) 
                     0))
         (max-chars (max 10 (- (/ text-w char-w) ln-width 2))))
    
    (dolist (item data)
      (let* ((start (plist-get item :start))
             (line-num (plist-get item :line))
             (content (with-current-buffer src-buf
                        (save-excursion
                          (goto-char start)
                          ;; 性能优化：只确保当前行的 font-lock
                          (font-lock-ensure (line-beginning-position) (line-end-position))
                          (buffer-substring (line-beginning-position) (line-end-position)))))
             (num-str (if (> ln-width 0) (format (format " %%%dd " ln-width) line-num) "")))
        (insert (propertize num-str 'face 'head-context-sticky-number-face)
                (propertize (truncate-string-to-width content (round max-chars) 0 nil "..")
                            'src-line-number line-num 
                            'keymap head-context-sticky-map 
                            'mouse-face 'highlight)
                "\n")))
    (when (> (buffer-size) 0) (delete-char -1))))

(defun head-context-sticky--update-all (&optional v)
  (head-context-sticky--gc)
  (walk-window-tree #'head-context-sticky--update-window))
;; -----------------------------------------------------------------------------
;; 优化：调度逻辑 (使用 Idle Timer)
;; -----------------------------------------------------------------------------

(defvar head-context-sticky--timer nil)
(defun head-context-sticky--schedule-update ()
  "PERF: 使用 Idle Timer 避免在输入时频繁触发计算。"
  (when head-context-sticky--timer (cancel-timer head-context-sticky--timer))
  (setq head-context-sticky--timer
        (run-with-idle-timer
         0.05 nil 
         (lambda ()
           (when (window-live-p (selected-window))
             (head-context-sticky--update-window (selected-window)))))))

;; -----------------------------------------------------------------------------
;; 6. Lifecycle
;; -----------------------------------------------------------------------------
(defun head-context-sticky--update-window-h (win &optional _) (head-context-sticky--update-window win))


;;;###autoload
(define-minor-mode head-context-sticky-mode
  "Sticky context header."
  :global nil
  (if head-context-sticky-mode
      (progn
        (add-hook 'post-command-hook #'head-context-sticky--schedule-update nil t)
        (add-hook 'window-size-change-functions #'head-context-sticky--update-all nil t)
        (add-hook 'window-configuration-change-hook #'head-context-sticky--update-all nil t)
        (add-hook 'kill-buffer-hook
                  (lambda () (mapc #'head-context-sticky--cleanup-window
                                   (get-buffer-window-list (current-buffer)))) nil t)
        (head-context-sticky--update-window (selected-window)))
    
    (remove-hook 'post-command-hook #'head-context-sticky--schedule-update t)
    (maphash (lambda (w s)
               (let ((frame (car s)) (sbuf (cdr s)))
                 (set-window-parameter w 'head-sticky-last-content nil)
                 (when (frame-live-p frame) (delete-frame frame))
                 (when (get-buffer (or sbuf "")) (kill-buffer sbuf))))
             head-context-sticky--registry)
    (clrhash head-context-sticky--registry)))

;;;###autoload
(define-globalized-minor-mode global-head-context-sticky-mode
  head-context-sticky-mode
  (lambda () (unless (minibufferp) (head-context-sticky-mode +1))))

(defun head-context-sticky-make-invisible () (setopt head-context-sticky-invisible t))
(defun head-context-sticky-make-visible () (setopt head-context-sticky-invisible nil))
(with-eval-after-load 'ediff
  (add-hook 'ediff-before-setup-hook #'head-context-sticky-make-invisible)
  (add-hook 'ediff-quit-hook #'head-context-sticky-make-visible))

(provide 'head-context-sticky)
