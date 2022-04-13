;;; mu.el --- Play on a MUSH or MUD

;; Copyright (C) 2022 Alex Schroeder <alex@gnu.org> && Pierce Wang <pierce.g.wang@gmail.com>

;; Emacs Lisp Archive Entry
;; Filename: mu.el
;; Version: 1.1
;; Keywords: comm, games
;; Homepage: https://github.com/piercegwang/mu.el
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Pierce Wang <pierce.g.wang@gmail.com>
;; Description: Play in a MUSH or MUD within Emacs.
;; Compatibility: Emacs27

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This is is a MUSH or MUD client.  Instead of using telnet or
;; standalone client to connect to your favorite MUSH or MUD, you can
;; use mu.el to play within Emacs.

;; I used to play using tinymud.el, but decided to rewrite it based on
;; comint-mode.  :)

;; Before playing, customize `mu-worlds'.  Then use `mu-open' to open a
;; connection to one of the worlds.  This will automaticall create a mu
;; connection buffer and a mu buffer.  You can type commands in either
;; buffer and send them to the host with RET.  The output will be in the
;; mu connection buffer.

;; If you load ansi-color.el, you should be able to get ANSI colors.

;;; Code:

(require 'comint)
(require 'cl-lib)
(load "ansi-color" t)

(defgroup mu nil
  "A MUSH or MUD client."
  :group 'processes)

(defcustom mu-worlds nil
  "List of worlds you play in.
You need to define the worlds you play in before you can get
started.  In most worlds, you can start playing using a guest account.

Each element WORLD of the list has the following form:

\[NAME HOST PORT CHARACTER PASSWORD]

NAME identifies the connection, HOST and PORT specify the network
connection, CHARACTER and PASSWORD are used to connect automatically.

Note that this will be saved in your `custom-file' -- including your
passwords!  If you don't want that, specify nil as your password."
  :type '(repeat
          (vector :tag "World"
                  (string :tag "Name")
                  (string :tag "Host")
                  (integer :tag "Port")
                  (string :tag "Char" :value "guest")
                  (repeat string :tag "Logins")))
  :group 'mu)

;; Accessing the fields

(defsubst mu-world-name (world)
  "Return the name for WORLD as a string."
  (concat (aref world 3) "@" (aref world 0)))

(defsubst mu-world-network (world)
  "Return the network details for WORLD as a cons cell (HOST . PORT)."
  (cons (aref world 1) (aref world 2)))

(defsubst mu-world-login-strings (world)
  "The list of login commands to send on start."
  (aref world 4))

;;; Modes

(defvar mu-input-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-parent)
        (set-keymap-parent map text-mode-map); Emacs
      (set-keymap-parents map (list text-mode-map))); XEmacs
    (if (functionp 'set-keymap-name)
        (set-keymap-name map 'mu-input-mode-map)); XEmacs
    (define-key map (kbd "<RET>") 'mu-send)
    map)
  "Mode map used for `mu-input-mode'.
Based on `text-mode-map'.")

(defvar mu-connection nil
  "Local variable for the connection.")

(defun mu-input-mode (&optional conn)
  "Major mode to type commands for the mu connection.
This is called a mu input buffer.

Use \\[mu-open] to open a connection.
Use \\[mu-choose-connection] to choose a connection.
Use \\[mu-send] to send commands to the current connection.

This function will run `mu-input-mode-hook' at the end.

\\{mu-input-mode-map}"
  (interactive)
  (setq conn (or conn mu-connection (mu-get-connection)))
  (kill-all-local-variables)
  (setq major-mode 'mu-input-mode)
  (setq mode-name "MU* Input")
  (use-local-map mu-input-mode-map)
  (defvar mu-world)
  (message "%s" (buffer-local-value 'mu-world conn))
  ;; Make each buffer in mu-input-mode remember the current connection.
  (let ((world (buffer-local-value 'mu-world conn)))
    (set (make-local-variable 'mu-connection) conn)
    (set (make-local-variable 'mu-world) world))
  ;; Run hook
  (run-hooks 'mu-input-mode-hook))

(defvar mu-connection-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-parent)
        (set-keymap-parent map comint-mode-map); Emacs
      (set-keymap-parents map (list comint-mode-map))); XEmacs
    (if (functionp 'set-keymap-name)
        (set-keymap-name map 'mu-connection-mode-map)); XEmacs
    ;; (define-key map (kbd "C-j") 'mu-accumulate-and-indent)
    map)
  "Mode map used for `mu-connection-mode'.
Based on `comint-mode-map'.")

(defvar mu-name nil
  "Local variable for the connection name.")

(defvar mu-world nil
  "Local variable for world and its data.")

(defun mu-connection-mode (name)
  "Major mode for a mu connection.

Use \\[comint-send-input] to send commands.
Use \\[mu-open] to open other connections.
Use \\[mu-input-buffer] to create a mu input buffer.

This function will run `mu-connection-mode-hook' at the end.

\\{mu-connection-mode-map}"
  (comint-mode)
  (setq major-mode 'mu-connection-mode)
  (setq mode-name "MU* Conn")
  (use-local-map mu-connection-mode-map)
  (set (make-local-variable 'mu-name) name)
  (add-to-list 'comint-output-filter-functions 'mu-insert-newline)
  (delete 'comint-watch-for-password-prompt comint-output-filter-functions)
  (add-to-list 'comint-input-filter-functions 'mu-debugger)
  ;; User stuff.
  (run-hooks 'mu-connection-mode-hook))

(put 'mu-connection-mode 'mode-class 'special)

(defun mu-login (world)
  "Login for WORLD in the current buffer. If non-nil, sends the
login strings one by one after a \": \" is detected."
  (let* ((logins (mu-world-login-strings world))
         (delay 2)
         (i 0)
         (stringcount (length logins)))
    (message "world: %s" world)
    (message "logins: %s" logins)
    (unless (eq logins nil)
      (while (< i stringcount)
        (run-at-time delay nil 'process-send-string mu-connection (format "%s\n" (nth i logins)))
        (message "string: %s\ndelay: %s\ni: %s" (nth i logins) delay i)
        (setq delay (+ delay 3))
        (setq i (+ i 1))))
    t))

;;; Opening connections

(defvar mu-world-history nil
  "History for `mu-get-world'.")

(defun mu-get-world ()
  "Let the user choose a world from `mu-worlds'.  
The return value is a cons cell, the car is the name of the connection,
the cdr holds the connection details from `mu-worlds'."
  (let ((world-completions
         (mapcar (lambda (w)
                   (cons (mu-world-name w) w))
                 mu-worlds)))
    (if world-completions
        (cdr (assoc (completing-read "World: " world-completions
                                     nil t nil mu-world-history)
                    world-completions))
      (customize-option 'mu-worlds)
      nil)))

(defun mu-open (world)
  "Create a new mu connection."
  (interactive (list (mu-get-world)))
  (when world
    (message "Opening connection...")
    (let ((buf (make-comint (mu-world-name world) (mu-world-network world))))
      (pop-to-buffer buf)
      (mu-connection-mode (mu-world-name world))
      (set (make-local-variable 'mu-world) world)
      (mu-input-buffer buf)
      (message "Opening connection...done"))))

(defun mu-reconnect (world)
  "Renew the connection in a mu output buffer."
  (interactive (list (mu-get-world)))
  (open-network-stream (mu-world-name world) (current-buffer)
                       (car (mu-world-network world))
                       (cdr (mu-world-network world))))


;; Creating mu mode buffers

(defun mu-input-buffer (buf)
  "Create a mu input buffer for connection BUF.
The current buffer must be a mu connection."
  (interactive (list (mu-get-connection)))
  (set-buffer buf)
  (pop-to-buffer (get-buffer-create
                  (concat "*Input for " mu-name "*")))
  (mu-input-mode buf))

(defvar mu-connection-history nil
  "History for `mu-get-connection'.")

(defun mu-get-connection ()
  "Let the user choose a connection from all buffers.
Only buffers with `mu-name' set are eligible.
Note that `default-value' of `mu-name' must be nil for this to work."
  (let ((buffers (buffer-list))
        buf conns)
    (while buffers
      (setq buf (car buffers)
            buffers (cdr buffers))
      (set-buffer buf)
      (when mu-name
        (setq conns (cons (cons mu-name buf) conns))))
    (cdr (assoc (completing-read "Connection: " conns 
                                 nil t nil mu-connection-history)
                conns))))

;; Sending stuff

(defun mu-send ()
  "Send current line to the current connection.
The current connection is stored in `mu-connection'."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (let ((str (buffer-substring-no-properties (point) pos)))
        (if (not (mu-command-filter str))
            (progn
              (unless mu-connection
                (error "No connection"))
              (process-send-string
               mu-connection
               (concat str "\n")))
          (message "Command called: str")))))
  (when (looking-at "\\'")
    (newline)))

;; Command filter

(defun mu-command-filter (str)
  "Filter string being sent before it gets to process."
  (when (and (> (length str) 1) (string= (substring str 0 1) "."))
    (let ((command (substring str 1)))
      (message "message command filter: %s" command)
      (cond ((string= command "login") (mu-login mu-world))
            ((string= command "quit") (kill-buffer mu-connection)))
      t)))

;; Receiving stuff

(defun mu-insert-newline (str)
  "Inserts a newline character between process outputs. Sometimes
MU*s don't do this properly."
  (save-excursion
    (let ((pos (point-marker)))
      (goto-char comint-last-output-start)
      (insert "\n"))))

(defun mu-debugger (str)
  "Print string"
  (message "input to process: %s" str)
  str)

(provide 'mu)

;;; mu.el ends here
