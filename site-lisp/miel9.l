;;; -*- Mode: Lisp -*-
;;; miel9.el  往年の著名な DOS-Filer Miel.exe の clone
;;; -------
;;; copyright (c)  2004-06-22 by Hideo HAGA
;;; revision 0.1   2004-06-27 by Hideo HAGA
;;; revision 0.2   2004-06-28 by Hideo HAGA
;;; revision 0.3   2004-06-30 by Hideo HAGA
;;; revision 0.4   2004-07-01 by Hideo HAGA
;;; revision 0.5   2004-07-07 by Hideo HAGA
;;; revision 0.6   2004-11-29 by Hideo HAGA       ; 構造の見直し
;;; revision 0.7   2004-11-29 by Hideo HAGA       ; 構造の再構築(基本機能)
;;; revision 0.8   2004-12-05 by Hideo HAGA       ; 追加機能検討
;;; revision 0.9   2006-10-08 by Hideo HAGA       ; カーソルを移動元に戻す対応
;;; revision 0.9.0 2009-06-21 by Hideo HAGA       ; xyzzyLispへの移植 09-06-21～
;;; revision 0.9.1 2009-06-30 by Shinichirou TADA ; 関数 miel-make-files-list のバグFIX

; ----------

;;; グローバル変数の初期設定

(defvar Miel-main-window nil)               ; メインウィンドウ
(defvar Miel-window-height 50)              ; ウィンドウの高さ
(defvar Miel-window-ratio 1/2)              ; ウィンドウの高さ比率
(defvar Miel-previous-dir " ")              ; 直前のdirectory
(defvar Miel-current-line 3)                ; 現在の行番号
(defvar Miel-current-directory nil)         ; 現在の対象ディレクトリ
(defvar Miel-edit-filename nil)             ; 編集対象ファイル名
(defvar Miel-edit-buffer nil)               ; 編集バッファ名一時記憶変数
(defvar Miel-mode-line-format               ; モードライン定義
 "--%*- %b (%M) [%k:%l] %P %f")
(defvar Miel-frame-name "miel")             ; フレーム名
(defvar Miel-pwd-buffer-name "[ Show Pwd ]")
(defvar Miel-browser-buffer-name "[ File Browser ]")
;  "( %M ) %P %f")

; ----------

;;; キーマップ設定

; 新規ローカルキーマップ
(defvar *miel-mode-map* nil)
(unless *miel-mode-map*
 (setq *miel-mode-map* (make-sparse-keymap))
 ;(suppress-keymap miel-mode-map)   ; 一般文字キーを全て無効化
 ; 移動
 (define-key *miel-mode-map* '(#\t)    'miel-top-line)
 (define-key *miel-mode-map* '(#\b)    'miel-bottom-line)
 (define-key *miel-mode-map* '(#\n)    'miel-next-line)
 (define-key *miel-mode-map* '(#\p)    'miel-previous-line)
 (define-key *miel-mode-map* '(#\Down) 'miel-next-line)
 (define-key *miel-mode-map* '(#\Up)   'miel-previous-line)
 ; ファイル編集・ディレクトリ移動
 (define-key *miel-mode-map* '(#\C-m)  'miel-find-file)
 ; マークの設定・解除
 (define-key *miel-mode-map* '(#\SPC)  'miel-mark-file)
 ; 削除
 (define-key *miel-mode-map* '(#\d)    'miel-delete-file))
 (define-key *miel-mode-map* '(#\q)    'miel-quit)
; kill-buffer キーバインドの再定義(global-map でないとダメ)
(define-key *global-keymap*   '(#\C-x #\k) 'miel-delete-buffer)

; ----------

;;; コマンド関数定義

(defun miel-delete-buffer ()
 ; Miel 版 C-xk (kill-buffer)
 (interactive)
 (cond
  ((equal Miel-edit-buffer nil)                     ; Mielが編集を起動していなければ
   (delete-other-windows)
   (cond
    ((find-name-buffer Miel-browser-buffer-name)
     (delete-buffer Miel-browser-buffer-name))
    (t nil))
;   (delete-buffer (selected-buffer)))               ; カレントバッファを削除
   )
  (t
   (delete-buffer Miel-edit-buffer)                 ; 編集バッファを削除
   (setq Miel-edit-buffer nil)                      ; リセット
   (miel (default-directory)))))                    ; Miel を再起動

; ----------

(defun // (x y)
 ; 割り算の結果余りを捨てた整数を返す
 ; 請負関数
 (floor (/ x y)))

(defun mode-attributes-string (file)
 ; attr ← (car (get-file-info file))
 ; ファイルのモードをあらわす文字列を返す
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

(defun miel-move-cursor-overlay ()
 ; 以前の行の属性を消して、現在行をハイライト
 (clear-all-text-attributes)
 (save-excursion
   (beginning-of-line)
   (let ((color))
     (if (equal #\d (progn (forward-char) (char-after (point))))
         ; ディレクトリなら
         (setq color 1) ; RED
       ; ファイルなら
       (setq color 4))  ; BLUE
     (set-text-attribute
      (progn (beginning-of-line) (point))
      (progn (end-of-line) (point))
      'line
      :bold t
      :foreground color
      :underline t))))

; ----------

(defun miel-top-line ()
 ; 先頭の行を表示する
 (interactive)
 (beginning-of-buffer)
 (miel-move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-bottom-line ()
 ; 最後の行を表示する
 (interactive)
 (end-of-buffer)
 (forward-line -1)
 (miel-move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-next-line ()
 ; 次の行を表示する
 (interactive)
 (forward-line)
 (miel-move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-previous-line ()
 ; 前の行を表示する
 (interactive)
 (forward-line -1)
 (miel-move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-goto-line (no)
 ; 指定行を表示する
 (interactive)
 (goto-line no)
 (miel-move-cursor-overlay)
 (miel-browse-file-head))

; ----------

(defun miel-mark-file ()
 ; 操作対象ファイル行にマークを設定・解除
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
     ; ディレクトリなら
     ; 今のところ何もしない
     )
    ((equal mark-flag " ")
     ; 未だマークがなかったらマークを追加
     (beginning-of-line)
     (delete-char 1)
     (insert "*"))
    (t
     ; 既にマークされていたらマークを削除
     (beginning-of-line)
     (delete-char 1)
     (insert " ")))
   (setq buffer-read-only t)
   (miel-next-line)))

; ----------

(defun miel-get-filename ()
 ; カレント行のファイル名を取得
 ; ret == list: (dir-flag filename)
 (let ((filename '()))
   (save-excursion
     ; 行末の非空白部分をファイル名と見なす
     (end-of-line)
     (let ((p2 (point))
           (p1 (progn
                 (skip-chars-backward "^ \t")
                 (point))))
       (setq filename (cons (buffer-substring p1 p2) filename))))
   (save-excursion
     ; 行頭の二文字目でディレクトリかどうか判断
     (beginning-of-line)
     (let ((dir-flag
            (progn
              (forward-char 1)
              (format nil "~C"
                      (char-after (point))))))
       (setq filename (cons dir-flag filename))))))

; ----------

(defun miel-find-file ()
 ; カレント行のファイルのオープン
 (interactive)
 (set-window Miel-main-window)
 (let ((file (miel-get-filename)))
   (cond
    ((equal "d" (car file))                   ; ディレクトリなら
     (setq Miel-previous-dir (default-directory))
     (miel (car (cdr file))))                 ; Miel を再起動
    (t                                        ; ファイルなら
     (setq Miel-current-line
           (current-line-number))             ; 現在行を記憶
     (delete-other-windows)                   ; win2 バッファを削除して
     (setq Miel-edit-buffer (car (cdr file))) ; 編集バッファ名を一時記憶
     (find-file Miel-edit-buffer)))))         ; ファイルを開く

; ----------

(defun miel-browse-file-head ()
 ; 現在行のファイルの内容を別ウィンドウに読み込み、表示する
 (interactive)
 (let ((win (selected-window))
       (buf2 (get-buffer-create Miel-browser-buffer-name))
       (file (miel-get-filename)))        ; ファイル名取得
   (save-excursion
     (set-buffer buf2)                    ; 編集対象バッファを切替える
     (erase-buffer (selected-buffer))     ; バッファをクリアする
     (cond
      ((equal "d" (car file))             ; ディレクトリなら
       (insert "\nこれはディレクトリです"))
      ((equal "-" (car file))             ; ファイルなら
       (insert-file-contents
        (concat Miel-current-directory (car (cdr file)))
        nil 0 (* 80 40)))                 ; 80桁*40行分表示
      (t                                  ; ディレクトリでもファイルでもない
       (insert "\nファイルが存在しません")))
     (other-window)
     (set-buffer buf2)
     (other-window))))                    ; 元のウィンドウに戻る

; -----

; この関数に何かしらバグ？あり。⇒　ホームディレクトリの親に移動しようとすると
; エラー（指定されたファイル../が見つかりません）になる⇒EmacsではOK？謎。
; 2009-06-28(Sun) by Hideo HAGA ⇒ ◎バグFIX 2009-06-30(Mon) by Shinichirou TADA

(defun miel-make-files-list ()
 ; ファイル一覧リストを生成する
 (let ((files (reverse (directory Miel-current-directory :show-dots t)))
       (mode nil) (size nil) (date nil) (file nil) (files-list nil))
   (while files
     (setq Miel-files2 files)
     (setq file (car files))
     (setq mode (mode-attributes-string file))
     (setq size (file-length file))
     ; ///// 以下、バグFIXのために処理追加 2009-06-30(Mon) by Shinichirou TADA
     (if (equal nil size) ; ドライブに対して file-length を実行すると nil が返る
         (progn
           (setq size 0)
           (setq date "00/00/00 00:00:00"))
       (progn
         ; ///// 以上、バグFIXのために処理追加 2009-06-30(Mon) by Shinichirou TADA
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
 ; ファイル一覧リストを表示する
 (let ((files-list (miel-make-files-list)))
   (while files-list
     (insert (car files-list))
     (setq files-list (cdr files-list)))))

; ----------

(defun frame-get ()
  ; Miel のフレームを取得
  (find-pseudo-frame Miel-frame-name))

; ----------

(defun restore ()
  ; Miel のフレームを表示
  (interactive)
  (let ((frame (frame-get)))
    (when frame
      (select-pseudo-frame frame)
      (when (find-buffer Miel-pwd-buffer-name)
        (switch-to-buffer Miel-pwd-buffer-name))
      )))

; ----------

(defun miel-quit ()
  ; miel-mode の終了
  (interactive)
  (let ((pf (find-pseudo-frame Miel-frame-name)))
    (when pf
      (delete-pseudo-frame pf)
      (when (find-buffer Miel-browser-buffer-name)
        (delete-buffer Miel-browser-buffer-name))
      (when (find-buffer Miel-pwd-buffer-name)
        (delete-buffer Miel-pwd-buffer-name))
      )))

; ----------

(defun miel (dir)
 ; Miel メイン処理関数
 ; ディレクトリ一覧を表示してファイルの情報を表示
 (setq buffer-read-only nil)
 (erase-buffer (selected-buffer))
 (cd dir)
 (setq Miel-current-directory (default-directory))
 (miel-list-files)
 ; カーソルを対象行に移動する
 (cond
  ((string= Miel-previous-dir " ")    ; ファイルを閉じた後の場合
   (miel-goto-line Miel-current-line) ; ファイルを開く前の行に戻す
   (setq Miel-current-line 3))        ; 使用後は初期値に戻す
  (t                                  ; ディレクトリ移動の場合
   (miel-goto-line Miel-current-line) ; 初期値のまま「3」のはず
   (setq Miel-current-line 3)         ; 使用後は初期値に戻す
   (search-forward
    (concat " " (file-namestring (substring Miel-previous-dir 0 -1)))
    t)
   (beginning-of-line)
   (setq Miel-previous-dir " ")))
 (miel-move-cursor-overlay)
 (delete-other-windows)
 (split-window (truncate (* Miel-window-height Miel-window-ratio)))
 (miel-browse-file-head)
 ; モードラインの設定
 (let ((str-dir (format nil " ~40@A   " (default-directory))))
   (setq mode-line-format (concat str-dir Miel-mode-line-format)))
 (update-mode-line)
 (setq buffer-read-only t))

; ----------

;;; Miel mode メジャーモード関数本体

(defun miel-mode ()
 ; 往年の著名な DOS-Filer Miel.exe の clone
 (interactive)
  (when (frame-get)
    (restore)
    (return-from miel-mode))
 (new-pseudo-frame Miel-frame-name)
 (setq Miel-window-height (window-height))
 (switch-to-buffer Miel-pwd-buffer-name)
 (setq Miel-main-window (selected-window))
 ; メジャーモードの設定
 (setq major-mode 'miel-mode)
 (setq mode-name "Miel mode")
 ; 専用キーマップの使用宣言
 (use-keymap *miel-mode-map*)
 ; Miel 実行。
 (miel "~/")
 ; メジャーモードフックを設定
 (run-hooks 'miel-mode-hook))
