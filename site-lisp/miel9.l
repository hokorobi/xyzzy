;;; -*- Mode: Lisp -*-
;;; miel9.el  ���N�̒����� DOS-Filer Miel.exe �� clone
;;; -------
;;; copyright (c)2004-06-22 by Hideo HAGA
;;; revision 0.1 2004-06-27 by Hideo HAGA
;;; revision 0.2 2004-06-28 by Hideo HAGA
;;; revision 0.3 2004-06-30 by Hideo HAGA
;;; revision 0.4 2004-07-01 by Hideo HAGA
;;; revision 0.5 2004-07-07 by Hideo HAGA
;;; revision 0.6 2004-11-29 by Hideo HAGA   ; �\���̌�����
;;; revision 0.7 2004-11-29 by Hideo HAGA   ; �\���̍č\�z(��{�@�\)
;;; revision 0.8 2004-12-05 by Hideo HAGA   ; �ǉ��@�\����
;;; revision 0.9 2006-10-08 by Hideo HAGA   ; �J�[�\�����ړ����ɖ߂��Ή�
;;; revision 1.0 2009-06-21 by Hideo HAGA   ; xyzzyLisp�ւ̈ڐA 09-06-21�`

; ----------

;;; �O���[�o���ϐ��̏����ݒ�

(defvar Miel-main-window nil)               ; ���C���E�B���h�E
(defvar Miel-window-height 50)              ; �E�B���h�E�̍���
(defvar Miel-window-ratio 1/2)              ; �E�B���h�E�̍����䗦
(defvar Miel-previous-dir " ")              ; ���O��directory
(defvar Miel-current-line 3)                ; ���݂̍s�ԍ�
(defvar Miel-current-directory nil)         ; ���݂̑Ώۃf�B���N�g��
(defvar Miel-edit-filename nil)             ; �ҏW�Ώۃt�@�C����
(defvar Miel-edit-buffer nil)               ; �ҏW�o�b�t�@���ꎞ�L���ϐ�
;(defvar Miel-cursor-overlay nil)            ; �J�[�\���I�[�o�[���C�I�u�W�F�N�g
;(defvar Miel-mode-line-strings nil)         ; ���[�h���C���\��������
(defvar Miel-mode-line-format               ; ���[�h���C����`
 "--%*- %b (%M) [%k:%l] %P %f")

; ----------

;;; �L�[�}�b�v�ݒ�

; �V�K���[�J���L�[�}�b�v
(defvar *miel-mode-map* nil)
(unless *miel-mode-map*
 (setq *miel-mode-map* (make-sparse-keymap))
 ;(suppress-keymap miel-mode-map)   ; ��ʕ����L�[��S�Ė�����
 ; �ړ�
 (define-key *miel-mode-map* '(#\t)    'miel-top-line)
 (define-key *miel-mode-map* '(#\b)    'miel-bottom-line)
 (define-key *miel-mode-map* '(#\n)    'miel-next-line)
 (define-key *miel-mode-map* '(#\p)    'miel-previous-line)
 (define-key *miel-mode-map* '(#\Down) 'miel-next-line)
 (define-key *miel-mode-map* '(#\Up)   'miel-previous-line)
 ; �t�@�C���ҏW�E�f�B���N�g���ړ�
 (define-key *miel-mode-map* '(#\C-m)  'miel-find-file)
 ; �}�[�N�̐ݒ�E����
 (define-key *miel-mode-map* '(#\SPC)  'miel-mark-file)
 ; �폜
 (define-key *miel-mode-map* '(#\d)    'miel-delete-file))
; kill-buffer �L�[�o�C���h�̍Ē�`(global-map �łȂ��ƃ_��)
(define-key *global-keymap*   '(#\C-x #\k) 'miel-delete-buffer)

; ----------

;;; �R�}���h�֐���`

(defun miel-delete-buffer ()
 ; Miel �� C-xk (kill-buffer)
 (interactive)
 (cond
  ((equal Miel-edit-buffer nil)                     ; Miel���ҏW���N�����Ă��Ȃ����
   (delete-other-windows)
   (cond
    ((find-name-buffer "[ File Browser ]")
     (delete-buffer "[ File Browser ]"))
    (t nil))
   (delete-buffer (selected-buffer)))               ; �J�����g�o�b�t�@���폜
  (t
   (delete-buffer Miel-edit-buffer)                 ; �ҏW�o�b�t�@���폜
   (setq Miel-edit-buffer nil)                      ; ���Z�b�g
   (miel (default-directory))
   )))                    ; Miel ���ċN��

; ----------

(defun // (x y)
 ; ����Z�̌��ʗ]����̂Ă�������Ԃ�
 ; �����֐�
 (floor (/ x y)))

(defun mode-attributes-string (file)
 ; attr �� (car (get-file-info file))
 ; �t�@�C���̃��[�h������킷�������Ԃ�
 ; directory  "d-----"
 ; readonly   "-r----"
 ; hidden     "--h---"
 ; system     "---s--"
 ; archive    "----a-"
 ; compressed "-----c"
 (interactive)
 (let ((attr nil) (x6 "-") (x5 "-") (x4 "-") (x3 "-") (x2 "-") (x1 "-"))
   (setq attr (get-file-attributes file))
   (if (= 1 (// attr 2048))
       (setq x1 "c"))
   (if (= 1 (// (mod attr 2048) 32))
       (setq x2 "a"))
   (if (= 1 (// (mod attr 32) 16))
       (setq x6 "d"))
   (if (= 1 (// (mod attr 16) 4))
       (setq x3 "s"))
   (if (= 1 (// (mod attr 4) 2))
       (setq x4 "h"))
   (if (= 1 (// (mod attr 2) 1))
       (setq x5 "r"))
   (concat x6 x5 x4 x3 x2 x1)))

; ----------

;(defun set-cursor-overlay ()
;  ; �J�[�\���I�[�o�[���C�̐ݒ�
;  (save-excursion
;    (setq Miel-cursor-overlay
;         (make-overlay (point) (progn (end-of-line) (point))))
;    (overlay-put
;     Miel-cursor-overlay
;     'face
;     'highlight)))

; ----------

;(defun move-cursor-overlay ()
;  ; �J�[�\���I�[�o�[���C���ړ�����
;  (save-excursion
;    (move-overlay
;     Miel-cursor-overlay
;     (point)
;     (progn (end-of-line) (point)))))

; ----------

(defun miel-top-line ()
 ; �擪�̍s��\������
 (interactive)
 (beginning-of-buffer)
;  (move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-bottom-line ()
 ; �Ō�̍s��\������
 (interactive)
 (end-of-buffer)
 (forward-line -1)
;  (move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-next-line ()
 ; ���̍s��\������
 (interactive)
 (forward-line)
;  (move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-previous-line ()
 ; �O�̍s��\������
 (interactive)
 (forward-line -1)
;  (move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-goto-line (no)
 ; �w��s��\������
 (interactive)
 (goto-line no)
;  (move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-mark-file ()
 ; ����Ώۃt�@�C���s�Ƀ}�[�N��ݒ�E����
 (interactive)
 (setq buffer-read-only nil)
 (beginning-of-line)
 (let ((mark-flag
        (format nil "~C"
         (char-after (point))))
       (dir-flag
        (progn
          (forward-char 1)
          (format nil "~C"
           (char-after (point))))))
   (cond
    ((equal dir-flag "d")
     ; �f�B���N�g���Ȃ�
     ; ���̂Ƃ��뉽�����Ȃ�
     )
    ((equal mark-flag " ")
     ; �����}�[�N���Ȃ�������}�[�N��ǉ�
     (beginning-of-line)
     (delete-char 1)
     (insert "*"))
    (t
     ; ���Ƀ}�[�N����Ă�����}�[�N���폜
     (beginning-of-line)
     (delete-char 1)
     (insert " ")))
   (setq buffer-read-only t)
   (miel-next-line)))

; ----------

(defun miel-get-filename ()
 ; �J�����g�s�̃t�@�C�������擾
 ; ret == list: (dir-flag filename)
 (let ((filename '()))
   (save-excursion
     ; �s���̔�󔒕������t�@�C�����ƌ��Ȃ�
     (end-of-line)
     (let ((p2 (point))
           (p1 (progn
                 (skip-chars-backward "^ \t")
                 (point))))
       (setq filename (cons (buffer-substring p1 p2) filename))))
   (save-excursion
     ; �s���̓񕶎��ڂŃf�B���N�g�����ǂ������f
     (beginning-of-line)
     (let ((dir-flag
            (progn
              (forward-char 1)
              (format nil "~C"
               (char-after (point))))))
       (setq filename (cons dir-flag filename))))))

; ----------

(defun miel-find-file ()
 ; �J�����g�s�̃t�@�C���̃I�[�v��
 (interactive)
 (set-window Miel-main-window)
 (let ((file (miel-get-filename)))
   (cond
    ((equal "d" (car file))                   ; �f�B���N�g���Ȃ�
     (setq Miel-previous-dir (default-directory))
     (miel (car (cdr file))))                 ; Miel ���ċN��
    (t                                        ; �t�@�C���Ȃ�
     (setq Miel-current-line
           (current-line-number))             ; ���ݍs���L��
     (delete-other-windows)                   ; win2 �o�b�t�@���폜����
     (setq Miel-edit-buffer (car (cdr file))) ; �ҏW�o�b�t�@�����ꎞ�L��
     (find-file Miel-edit-buffer)))))         ; �t�@�C�����J��

; ----------

(defun miel-browse-file-head ()
 ; ���ݍs�̃t�@�C���̓��e��ʃE�B���h�E�ɓǂݍ��݁A�\������
 (interactive)
 (let ((win (selected-window))
       (buf2 (get-buffer-create "[ File Browser ]"))
       (file (miel-get-filename)))        ; �t�@�C�����擾
   (save-excursion
     (set-buffer buf2)                    ; �ҏW�Ώۃo�b�t�@��ؑւ���
     (erase-buffer (selected-buffer))     ; �o�b�t�@���N���A����
     (cond
      ((equal "d" (car file))             ; �f�B���N�g���Ȃ�
       (insert "\n����̓f�B���N�g���ł�"))
      ((equal "-" (car file))             ; �t�@�C���Ȃ�
       (insert-file-contents
        (concat Miel-current-directory (car (cdr file)))
        nil 0 (* 80 40)))                 ; 80��*40�s���\��
      (t                                  ; �f�B���N�g���ł��t�@�C���ł��Ȃ�
       (insert "\n�t�@�C�������݂��܂���")))
     (other-window)
     (set-buffer buf2)
     (other-window))))                    ; ���̃E�B���h�E�ɖ߂�

; -----

; ���̊֐��ɉ�������o�O�H����B�ˁ@�z�[���f�B���N�g���̐e�Ɉړ����悤�Ƃ����
; �G���[�i�w�肳�ꂽ�t�@�C��../��������܂���j�ɂȂ��Emacs�ł�OK�H��B
; 2009-06-28(Sun) by Hideo HAGA

(defun miel-make-files-list ()
 ; �t�@�C���ꗗ���X�g�𐶐�����
 (let ((files (reverse (directory Miel-current-directory :show-dots t)))
       (mode nil) (size nil) (date nil) (file nil) (files-list nil))
   (while files
     (setq Miel-files2 files)
     (setq file (car files))
     (setq mode (mode-attributes-string file))
     (setq size (file-length file))
     (if (equal nil size)
         (progn
           (setq size 0)
           (setq date "00/00/00 00:00:00"))
       (progn
         (if (> (parse-integer (format nil "~A" size)) 99999999)
             (setq size "99999999"))
         (setq date (format-date-string "%y/%m/%d %H:%M:%S" (cadr (get-file-info file))))
         ))
     (setq files-list
           (cons (format nil " ~A ~8@A ~A ~A\n" mode size date file) files-list))
     (setq files (cdr files)))
   files-list))

; -----

(defun miel-list-files ()
 ; �t�@�C���ꗗ���X�g��\������
 (let ((files-list (miel-make-files-list)))
   (while files-list
     (insert (car files-list))
     (setq files-list (cdr files-list)))))

; ----------

(defun miel (dir)
 ; Miel ���C�������֐�
 ; �f�B���N�g���ꗗ��\�����ăt�@�C���̏���\��
 (setq buffer-read-only nil)
 (erase-buffer (selected-buffer))
 (cd dir)
 (setq Miel-current-directory (default-directory))
 (miel-list-files)
;  (set-cursor-overlay)
 ; �J�[�\����Ώۍs�Ɉړ�����
 (cond
  ((string= Miel-previous-dir " ")    ; �t�@�C���������̏ꍇ
   (miel-goto-line Miel-current-line) ; �t�@�C�����J���O�̍s�ɖ߂�
   (setq Miel-current-line 3))        ; �g�p��͏����l�ɖ߂�
  (t                                  ; �f�B���N�g���ړ��̏ꍇ
   (miel-goto-line Miel-current-line) ; �����l�̂܂܁u3�v�̂͂�
   (setq Miel-current-line 3)         ; �g�p��͏����l�ɖ߂�
   (search-forward
    (concat " " (file-namestring (substring Miel-previous-dir 0 -1)))
    t)
   (beginning-of-line)
   (setq Miel-previous-dir " ")))
;  (move-cursor-overlay)
 (delete-other-windows)
 (split-window (truncate (* Miel-window-height Miel-window-ratio)))
 (miel-browse-file-head)
 ; ���[�h���C���̐ݒ�
 (let ((str-dir))
   (setq mode-line-format (concat str-dir Miel-mode-line-format)))
 (update-mode-line)
 (setq buffer-read-only t))

; ----------

;;; Miel mode ���W���[���[�h�֐��{��

(defun miel-mode ()
 ; ���N�� DOS-Filer Miel.exe ���h�L
 (interactive)
 (delete-other-windows)
 (setq Miel-window-height (window-height))
 (switch-to-buffer "[ Show Pwd ]")
 (setq Miel-main-window (selected-window))
 ; ���W���[���[�h�̐ݒ�
 (setq major-mode 'miel-mode)
 (setq mode-name "Miel mode")
 ; ��p�L�[�}�b�v�̎g�p�錾
 (use-keymap *miel-mode-map*)
 ; Miel ���s�B
 (miel "~/")
 ; ���W���[���[�h�t�b�N��ݒ�
 (run-hooks 'miel-mode-hook))