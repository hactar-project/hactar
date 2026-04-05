;;;; tui-theme.lisp — Theme system for the Hactar TUI
;;;; Provides hex-color support, dynamic color pair registry,
;;;; sidebar widget system, and built-in themes.

(in-package :hactar)

;;; ============================================================
;;; Theme struct
;;; ============================================================

(defstruct tui-theme
  (name "default" :type string)
  ;; Base colors (hex strings like "#282828", NIL = terminal default)
  (bg              nil)
  (fg              nil)
  (border          "#a89984")
  (border-accent   "#83a598")
  ;; Semantic roles
  (header          "#d3869b")
  (header-bold     "#d3869b")
  (selected-fg     "#282828")
  (selected-bg     "#d3869b")
  (muted           "#928374")
  (prompt-marker   "#83a598")
  ;; Status bar
  (status-info-fg  "#ebdbb2")
  (status-info-bg  nil)
  (status-ok-fg    "#282828")
  (status-ok-bg    "#b8bb26")
  (status-error-fg "#ebdbb2")
  (status-error-bg "#fb4934")
  ;; Sidebar
  (sidebar-title   "#d3869b")
  (sidebar-label   "#83a598")
  ;; File indicators
  (file-added      "#b8bb26")
  (file-removed    "#fb4934")
  ;; Modal / command palette
  (modal-bg-fg     "#ebdbb2")
  (modal-bg-bg     "#3c3836")
  (modal-border    "#83a598")
  (modal-input-fg  "#ebdbb2")
  (modal-input-bg  "#504945")
  ;; Tabs
  (tab-active-fg   "#282828")
  (tab-active-bg   "#d3869b")
  (tab-inactive-fg "#a89984")
  (tab-inactive-bg nil)
  ;; Key hints
  (keyhint         "#83a598")
  ;; LSP / tool call status
  (lsp-ok          "#b8bb26")
  (tool-pending    "#fabd2f")
  (tool-running    "#83a598")
  (tool-done       "#b8bb26")
  (tool-failed     "#fb4934")
  ;; Chat message types
  (chat-user       "#83a598")
  (chat-assistant  nil)
  (chat-thought    "#928374")
  (chat-bash       "#b8bb26")
  (chat-error      "#fb4934")
  ;; Completion dropdown
  (completion-fg   "#ebdbb2")
  (completion-bg   "#3c3836"))

;;; ============================================================
;;; Global theme state
;;; ============================================================

(defvar *tui-theme* nil "The currently active tui-theme struct.")
(defvar *tui-theme-name* nil "Name of the theme to load at startup.")

(defvar *tui-color-pairs* (make-hash-table :test 'eq)
  "Maps semantic role keywords to ncurses color-pair IDs.")

(defvar *tui-next-color-id* 30
  "Next available ncurses color ID for custom colors (avoid 0-15 basic).")

(defvar *tui-next-pair-id* 1
  "Next available ncurses pair ID.")

(defvar *tui-color-cache* (make-hash-table :test 'equal)
  "Cache mapping hex color strings to resolved ncurses color IDs.")

;;; ============================================================
;;; Hex-to-color conversion
;;; ============================================================

(cffi:defcvar ("COLORS" %ncurses-colors) :int)

(defun ncurses-colors ()
  "Return the number of colors the terminal supports (ncurses COLORS variable)."
  %ncurses-colors)

(defun hex-to-rgb (hex)
  "Parse a hex color string like \"#RRGGBB\" to (values r g b) each 0-255.
   Also accepts \"RRGGBB\" without the leading #."
  (let ((s (if (and (> (length hex) 0) (char= (char hex 0) #\#))
               (subseq hex 1)
               hex)))
    (unless (= (length s) 6)
      (error "Invalid hex color: ~A (expected 6 hex digits)" hex))
    (values (parse-integer s :start 0 :end 2 :radix 16)
            (parse-integer s :start 2 :end 4 :radix 16)
            (parse-integer s :start 4 :end 6 :radix 16))))

(defun rgb-to-ncurses-1000 (r g b)
  "Scale 0-255 RGB to 0-1000 for ncurses init-color."
  (values (round (* r 1000) 255)
          (round (* g 1000) 255)
          (round (* b 1000) 255)))

(defun color-distance-squared (r1 g1 b1 r2 g2 b2)
  "Compute squared Euclidean distance between two RGB colors."
  (+ (* (- r1 r2) (- r1 r2))
     (* (- g1 g2) (- g1 g2))
     (* (- b1 b2) (- b1 b2))))

(defvar *xterm-256-colors* nil
  "Cached vector of (r g b) lists for xterm-256 palette indices 0-255.")

(defun build-xterm-256-palette ()
  "Build the xterm-256 color palette RGB values."
  (let ((palette (make-array 256 :initial-element nil)))
    ;; Standard colors 0-15 (approximate)
    (let ((basics #((0 0 0) (128 0 0) (0 128 0) (128 128 0)
                    (0 0 128) (128 0 128) (0 128 128) (192 192 192)
                    (128 128 128) (255 0 0) (0 255 0) (255 255 0)
                    (0 0 255) (255 0 255) (0 255 255) (255 255 255))))
      (loop for i from 0 below 16
            do (setf (aref palette i) (aref basics i))))
    ;; 216 color cube: indices 16-231
    (loop for i from 0 below 216
          for b = (mod i 6)
          for g = (mod (floor i 6) 6)
          for r = (floor i 36)
          do (setf (aref palette (+ 16 i))
                   (list (if (zerop r) 0 (+ 55 (* r 40)))
                         (if (zerop g) 0 (+ 55 (* g 40)))
                         (if (zerop b) 0 (+ 55 (* b 40))))))
    ;; Grayscale ramp: indices 232-255
    (loop for i from 0 below 24
          for v = (+ 8 (* i 10))
          do (setf (aref palette (+ 232 i)) (list v v v)))
    palette))

(defun get-xterm-256-palette ()
  "Get or build the xterm-256 palette."
  (unless *xterm-256-colors*
    (setf *xterm-256-colors* (build-xterm-256-palette)))
  *xterm-256-colors*)

(defun rgb-to-nearest-256 (r g b)
  "Find the closest xterm-256 color index for the given RGB values (0-255)."
  (let ((palette (get-xterm-256-palette))
        (best-index 0)
        (best-dist most-positive-fixnum))
    (loop for i from 0 below 256
          for entry = (aref palette i)
          when entry
          do (let ((dist (color-distance-squared r g b
                                                  (first entry) (second entry) (third entry))))
               (when (< dist best-dist)
                 (setf best-dist dist)
                 (setf best-index i))))
    best-index))

(defun rgb-to-nearest-8 (r g b)
  "Map RGB to the nearest basic 8-color ANSI index (0-7)."
  (let ((basics #((0 0 0) (128 0 0) (0 128 0) (128 128 0)
                  (0 0 128) (128 0 128) (0 128 128) (192 192 192)))
        (best-index 0)
        (best-dist most-positive-fixnum))
    (loop for i from 0 below 8
          for entry = (aref basics i)
          do (let ((dist (color-distance-squared r g b
                                                  (first entry) (second entry) (third entry))))
               (when (< dist best-dist)
                 (setf best-dist dist)
                 (setf best-index i))))
    best-index))

;;; ============================================================
;;; Color resolution
;;; ============================================================

(defun tui-resolve-color (hex-or-nil)
  "Resolve a hex color string to an ncurses color ID.
   NIL means -1 (terminal default).
   Uses init-color when can-change-color is true and COLORS >= 256,
   otherwise maps to nearest 256 or 8 color index."
  (when (null hex-or-nil)
    (return-from tui-resolve-color -1))
  ;; Check cache
  (let ((cached (gethash hex-or-nil *tui-color-cache*)))
    (when cached (return-from tui-resolve-color cached)))
  (multiple-value-bind (r g b) (hex-to-rgb hex-or-nil)
    (let ((color-id
            (cond
              ;; Best case: terminal supports redefining colors
              ((and (not (zerop (charms/ll:can-change-color)))
                    (>= (ncurses-colors) 256))
               (let ((id *tui-next-color-id*))
                 (incf *tui-next-color-id*)
                 (multiple-value-bind (nr ng nb) (rgb-to-ncurses-1000 r g b)
                   (charms/ll:init-color id nr ng nb))
                 id))
              ;; 256 color terminal but can't redefine: map to nearest
              ((>= (ncurses-colors) 256)
               (rgb-to-nearest-256 r g b))
              ;; 8 color fallback
              (t
               (rgb-to-nearest-8 r g b)))))
      (setf (gethash hex-or-nil *tui-color-cache*) color-id)
      color-id)))

;;; ============================================================
;;; Color pair registry
;;; ============================================================

(defun tui-register-pair (role fg-hex bg-hex)
  "Register a color pair for ROLE (a keyword). Returns the pair ID."
  (let ((fg-id (tui-resolve-color fg-hex))
        (bg-id (tui-resolve-color bg-hex))
        (pair-id *tui-next-pair-id*))
    (incf *tui-next-pair-id*)
    (charms/ll:init-pair pair-id fg-id bg-id)
    (setf (gethash role *tui-color-pairs*) pair-id)
    pair-id))

(defun tui-color-pair (role)
  "Look up the ncurses color-pair ID for a semantic ROLE keyword.
   Returns 0 (default) if the role is not registered."
  (gethash role *tui-color-pairs* 0))

;;; ============================================================
;;; Apply theme
;;; ============================================================

(defun tui-apply-theme (theme)
  "Apply a tui-theme struct: resolve all hex colors and register ncurses pairs.
   Must be called after ncurses is initialized (start-color, use-default-colors)."
  (charms/ll:start-color)
  (charms/ll:use-default-colors)
  ;; Reset registries
  (clrhash *tui-color-pairs*)
  (clrhash *tui-color-cache*)
  (setf *tui-next-color-id* 30)
  (setf *tui-next-pair-id* 1)
  (setf *tui-theme* theme)
  ;; Register all semantic color pairs
  (tui-register-pair :border       (tui-theme-border theme)       (tui-theme-bg theme))
  (tui-register-pair :header       (tui-theme-header theme)       (tui-theme-bg theme))
  (tui-register-pair :selected     (tui-theme-selected-fg theme)  (tui-theme-selected-bg theme))
  (tui-register-pair :status-ok    (tui-theme-status-ok-fg theme) (tui-theme-status-ok-bg theme))
  (tui-register-pair :status-error (tui-theme-status-error-fg theme) (tui-theme-status-error-bg theme))
  (tui-register-pair :status-info  (tui-theme-status-info-fg theme) (tui-theme-status-info-bg theme))
  (tui-register-pair :sidebar-title (tui-theme-sidebar-title theme) (tui-theme-bg theme))
  (tui-register-pair :sidebar-label (tui-theme-sidebar-label theme) (tui-theme-bg theme))
  (tui-register-pair :muted        (tui-theme-muted theme)         (tui-theme-bg theme))
  (tui-register-pair :file-added   (tui-theme-file-added theme)    (tui-theme-bg theme))
  (tui-register-pair :file-removed (tui-theme-file-removed theme)  (tui-theme-bg theme))
  (tui-register-pair :prompt-marker (tui-theme-prompt-marker theme) (tui-theme-bg theme))
  (tui-register-pair :modal-bg     (tui-theme-modal-bg-fg theme)   (tui-theme-modal-bg-bg theme))
  (tui-register-pair :modal-border (tui-theme-modal-border theme)  (tui-theme-bg theme))
  (tui-register-pair :modal-input  (tui-theme-modal-input-fg theme) (tui-theme-modal-input-bg theme))
  (tui-register-pair :tab-active   (tui-theme-tab-active-fg theme) (tui-theme-tab-active-bg theme))
  (tui-register-pair :tab-inactive (tui-theme-tab-inactive-fg theme) (tui-theme-tab-inactive-bg theme))
  (tui-register-pair :keyhint      (tui-theme-keyhint theme)       (tui-theme-bg theme))
  (tui-register-pair :lsp-ok       (tui-theme-lsp-ok theme)        (tui-theme-bg theme))
  (tui-register-pair :tool-pending (tui-theme-tool-pending theme)  (tui-theme-bg theme))
  (tui-register-pair :tool-running (tui-theme-tool-running theme)  (tui-theme-bg theme))
  (tui-register-pair :tool-done    (tui-theme-tool-done theme)     (tui-theme-bg theme))
  (tui-register-pair :tool-failed  (tui-theme-tool-failed theme)   (tui-theme-bg theme))
  ;; Chat-specific pairs
  (tui-register-pair :chat-user      (tui-theme-chat-user theme)      (tui-theme-bg theme))
  (tui-register-pair :chat-assistant (tui-theme-chat-assistant theme) (tui-theme-bg theme))
  (tui-register-pair :chat-thought   (tui-theme-chat-thought theme)   (tui-theme-bg theme))
  (tui-register-pair :chat-bash      (tui-theme-chat-bash theme)      (tui-theme-bg theme))
  (tui-register-pair :chat-error     (tui-theme-chat-error theme)     (tui-theme-bg theme))
  ;; Completion dropdown
  (tui-register-pair :completion     (tui-theme-completion-fg theme)  (tui-theme-completion-bg theme))
  theme)

;;; ============================================================
;;; Built-in themes
;;; ============================================================

(defun make-default-theme ()
  "Create the default theme using basic terminal colors (no hex, maps to defaults)."
  (make-tui-theme
   :name "default"
   :bg nil :fg nil
   :border nil
   :border-accent nil
   :header "#d787af"
   :header-bold "#d787af"
   :selected-fg "#000000" :selected-bg "#d787af"
   :muted "#808080"
   :prompt-marker "#00afaf"
   :status-info-fg nil :status-info-bg nil
   :status-ok-fg "#000000" :status-ok-bg "#00af00"
   :status-error-fg "#ffffff" :status-error-bg "#af0000"
   :sidebar-title "#d787af"
   :sidebar-label "#00afaf"
   :file-added "#00af00" :file-removed "#af0000"
   :modal-bg-fg "#ffffff" :modal-bg-bg "#000000"
   :modal-border "#00afaf"
   :modal-input-fg "#ffffff" :modal-input-bg "#000000"
   :tab-active-fg "#ffffff" :tab-active-bg "#d787af"
   :tab-inactive-fg "#ffffff" :tab-inactive-bg nil
   :keyhint "#00afaf"
   :lsp-ok "#00af00"
   :tool-pending "#afaf00" :tool-running "#00afaf"
   :tool-done "#00af00" :tool-failed "#af0000"
   :chat-user "#00afaf" :chat-assistant nil
   :chat-thought "#808080" :chat-bash "#00af00" :chat-error "#af0000"
   :completion-fg "#ffffff" :completion-bg "#000000"))

(defun make-gruvbox-dark-theme ()
  "Create the Gruvbox Dark theme."
  (make-tui-theme
   :name "gruvbox-dark"
   :bg "#282828" :fg "#ebdbb2"
   :border "#a89984" :border-accent "#83a598"
   :header "#d3869b" :header-bold "#d3869b"
   :selected-fg "#282828" :selected-bg "#d3869b"
   :muted "#928374" :prompt-marker "#83a598"
   :status-info-fg "#ebdbb2" :status-info-bg nil
   :status-ok-fg "#282828" :status-ok-bg "#b8bb26"
   :status-error-fg "#ebdbb2" :status-error-bg "#fb4934"
   :sidebar-title "#d3869b" :sidebar-label "#83a598"
   :file-added "#b8bb26" :file-removed "#fb4934"
   :modal-bg-fg "#ebdbb2" :modal-bg-bg "#3c3836"
   :modal-border "#83a598"
   :modal-input-fg "#ebdbb2" :modal-input-bg "#504945"
   :tab-active-fg "#282828" :tab-active-bg "#d3869b"
   :tab-inactive-fg "#a89984" :tab-inactive-bg nil
   :keyhint "#83a598"
   :lsp-ok "#b8bb26"
   :tool-pending "#fabd2f" :tool-running "#83a598"
   :tool-done "#b8bb26" :tool-failed "#fb4934"
   :chat-user "#83a598" :chat-assistant nil
   :chat-thought "#928374" :chat-bash "#b8bb26" :chat-error "#fb4934"
   :completion-fg "#ebdbb2" :completion-bg "#3c3836"))

(defun make-gruvbox-light-theme ()
  "Create the Gruvbox Light theme."
  (make-tui-theme
   :name "gruvbox-light"
   :bg "#fbf1c7" :fg "#3c3836"
   :border "#928374" :border-accent "#076678"
   :header "#8f3f71" :header-bold "#8f3f71"
   :selected-fg "#fbf1c7" :selected-bg "#8f3f71"
   :muted "#928374" :prompt-marker "#076678"
   :status-info-fg "#3c3836" :status-info-bg nil
   :status-ok-fg "#fbf1c7" :status-ok-bg "#79740e"
   :status-error-fg "#fbf1c7" :status-error-bg "#9d0006"
   :sidebar-title "#8f3f71" :sidebar-label "#076678"
   :file-added "#79740e" :file-removed "#9d0006"
   :modal-bg-fg "#3c3836" :modal-bg-bg "#ebdbb2"
   :modal-border "#076678"
   :modal-input-fg "#3c3836" :modal-input-bg "#d5c4a1"
   :tab-active-fg "#fbf1c7" :tab-active-bg "#8f3f71"
   :tab-inactive-fg "#928374" :tab-inactive-bg nil
   :keyhint "#076678"
   :lsp-ok "#79740e"
   :tool-pending "#b57614" :tool-running "#076678"
   :tool-done "#79740e" :tool-failed "#9d0006"
   :chat-user "#076678" :chat-assistant nil
   :chat-thought "#928374" :chat-bash "#79740e" :chat-error "#9d0006"
   :completion-fg "#3c3836" :completion-bg "#ebdbb2"))

(defun make-dracula-theme ()
  "Create the Dracula theme."
  (make-tui-theme
   :name "dracula"
   :bg "#282a36" :fg "#f8f8f2"
   :border "#6272a4" :border-accent "#bd93f9"
   :header "#ff79c6" :header-bold "#ff79c6"
   :selected-fg "#282a36" :selected-bg "#ff79c6"
   :muted "#6272a4" :prompt-marker "#bd93f9"
   :status-info-fg "#f8f8f2" :status-info-bg nil
   :status-ok-fg "#282a36" :status-ok-bg "#50fa7b"
   :status-error-fg "#f8f8f2" :status-error-bg "#ff5555"
   :sidebar-title "#ff79c6" :sidebar-label "#bd93f9"
   :file-added "#50fa7b" :file-removed "#ff5555"
   :modal-bg-fg "#f8f8f2" :modal-bg-bg "#44475a"
   :modal-border "#bd93f9"
   :modal-input-fg "#f8f8f2" :modal-input-bg "#44475a"
   :tab-active-fg "#282a36" :tab-active-bg "#ff79c6"
   :tab-inactive-fg "#6272a4" :tab-inactive-bg nil
   :keyhint "#bd93f9"
   :lsp-ok "#50fa7b"
   :tool-pending "#f1fa8c" :tool-running "#8be9fd"
   :tool-done "#50fa7b" :tool-failed "#ff5555"
   :chat-user "#bd93f9" :chat-assistant nil
   :chat-thought "#6272a4" :chat-bash "#50fa7b" :chat-error "#ff5555"
   :completion-fg "#f8f8f2" :completion-bg "#44475a"))

(defun make-catppuccin-mocha-theme ()
  "Create the Catppuccin Mocha theme."
  (make-tui-theme
   :name "catppuccin-mocha"
   :bg "#1e1e2e" :fg "#cdd6f4"
   :border "#585b70" :border-accent "#89b4fa"
   :header "#f5c2e7" :header-bold "#f5c2e7"
   :selected-fg "#1e1e2e" :selected-bg "#f5c2e7"
   :muted "#6c7086" :prompt-marker "#89b4fa"
   :status-info-fg "#cdd6f4" :status-info-bg nil
   :status-ok-fg "#1e1e2e" :status-ok-bg "#a6e3a1"
   :status-error-fg "#cdd6f4" :status-error-bg "#f38ba8"
   :sidebar-title "#f5c2e7" :sidebar-label "#89b4fa"
   :file-added "#a6e3a1" :file-removed "#f38ba8"
   :modal-bg-fg "#cdd6f4" :modal-bg-bg "#313244"
   :modal-border "#89b4fa"
   :modal-input-fg "#cdd6f4" :modal-input-bg "#45475a"
   :tab-active-fg "#1e1e2e" :tab-active-bg "#f5c2e7"
   :tab-inactive-fg "#6c7086" :tab-inactive-bg nil
   :keyhint "#89b4fa"
   :lsp-ok "#a6e3a1"
   :tool-pending "#f9e2af" :tool-running "#89b4fa"
   :tool-done "#a6e3a1" :tool-failed "#f38ba8"
   :chat-user "#89b4fa" :chat-assistant nil
   :chat-thought "#6c7086" :chat-bash "#a6e3a1" :chat-error "#f38ba8"
   :completion-fg "#cdd6f4" :completion-bg "#313244"))

(defun make-solarized-dark-theme ()
  "Create the Solarized Dark theme."
  (make-tui-theme
   :name "solarized-dark"
   :bg "#002b36" :fg "#839496"
   :border "#586e75" :border-accent "#268bd2"
   :header "#d33682" :header-bold "#d33682"
   :selected-fg "#fdf6e3" :selected-bg "#d33682"
   :muted "#586e75" :prompt-marker "#268bd2"
   :status-info-fg "#839496" :status-info-bg nil
   :status-ok-fg "#002b36" :status-ok-bg "#859900"
   :status-error-fg "#fdf6e3" :status-error-bg "#dc322f"
   :sidebar-title "#d33682" :sidebar-label "#268bd2"
   :file-added "#859900" :file-removed "#dc322f"
   :modal-bg-fg "#839496" :modal-bg-bg "#073642"
   :modal-border "#268bd2"
   :modal-input-fg "#839496" :modal-input-bg "#073642"
   :tab-active-fg "#fdf6e3" :tab-active-bg "#d33682"
   :tab-inactive-fg "#586e75" :tab-inactive-bg nil
   :keyhint "#268bd2"
   :lsp-ok "#859900"
   :tool-pending "#b58900" :tool-running "#2aa198"
   :tool-done "#859900" :tool-failed "#dc322f"
   :chat-user "#268bd2" :chat-assistant nil
   :chat-thought "#586e75" :chat-bash "#859900" :chat-error "#dc322f"
   :completion-fg "#839496" :completion-bg "#073642"))

(defun make-nord-theme ()
  "Create the Nord theme."
  (make-tui-theme
   :name "nord"
   :bg "#2e3440" :fg "#d8dee9"
   :border "#4c566a" :border-accent "#81a1c1"
   :header "#b48ead" :header-bold "#b48ead"
   :selected-fg "#2e3440" :selected-bg "#b48ead"
   :muted "#4c566a" :prompt-marker "#81a1c1"
   :status-info-fg "#d8dee9" :status-info-bg nil
   :status-ok-fg "#2e3440" :status-ok-bg "#a3be8c"
   :status-error-fg "#d8dee9" :status-error-bg "#bf616a"
   :sidebar-title "#b48ead" :sidebar-label "#81a1c1"
   :file-added "#a3be8c" :file-removed "#bf616a"
   :modal-bg-fg "#d8dee9" :modal-bg-bg "#3b4252"
   :modal-border "#81a1c1"
   :modal-input-fg "#d8dee9" :modal-input-bg "#434c5e"
   :tab-active-fg "#2e3440" :tab-active-bg "#b48ead"
   :tab-inactive-fg "#4c566a" :tab-inactive-bg nil
   :keyhint "#81a1c1"
   :lsp-ok "#a3be8c"
   :tool-pending "#ebcb8b" :tool-running "#88c0d0"
   :tool-done "#a3be8c" :tool-failed "#bf616a"
   :chat-user "#81a1c1" :chat-assistant nil
   :chat-thought "#4c566a" :chat-bash "#a3be8c" :chat-error "#bf616a"
   :completion-fg "#d8dee9" :completion-bg "#3b4252"))

;;; ============================================================
;;; Built-in theme registry
;;; ============================================================

(defvar *tui-builtin-themes*
  (list #'make-default-theme
        #'make-gruvbox-dark-theme
        #'make-gruvbox-light-theme
        #'make-dracula-theme
        #'make-catppuccin-mocha-theme
        #'make-solarized-dark-theme
        #'make-nord-theme)
  "List of factory functions for built-in themes.")

(defun list-available-themes ()
  "Return a list of available theme structs (built-in + user files)."
  (let ((themes (mapcar #'funcall *tui-builtin-themes*)))
    ;; Also load from ~/.config/hactar/themes/
    (let ((themes-dir (uiop:subpathname *hactar-config-path* "themes/")))
      (when (uiop:directory-exists-p themes-dir)
        (dolist (file (uiop:directory-files themes-dir))
          (when (and (stringp (pathname-type file))
                     (string-equal (pathname-type file) "lisp"))
            (handler-case
                (let ((loaded-theme (load-theme-from-file file)))
                  (when loaded-theme
                    (push loaded-theme themes)))
              (error (e)
                (format *error-output* "~&Warning: Failed to load theme ~A: ~A~%"
                        (uiop:native-namestring file) e)))))))
    themes))

(defun find-theme-by-name (name)
  "Find and return a theme by NAME. Searches built-in themes first, then files.
   Returns a tui-theme struct or NIL."
  (dolist (factory *tui-builtin-themes*)
    (let ((theme (funcall factory)))
      (when (string-equal name (tui-theme-name theme))
        (return-from find-theme-by-name theme))))
  ;; Try loading from file
  (let ((themes-dir (uiop:subpathname *hactar-config-path* "themes/")))
    (when (uiop:directory-exists-p themes-dir)
      (let ((file (merge-pathnames (make-pathname :name name :type "lisp") themes-dir)))
        (when (probe-file file)
          (return-from find-theme-by-name (load-theme-from-file file))))))
  ;; Try as a full path
  (when (and (pathnamep (pathname name)) (probe-file name))
    (return-from find-theme-by-name (load-theme-from-file name)))
  nil)

(defun load-theme-from-file (path)
  "Load a theme from a Lisp file. The file should evaluate to a tui-theme struct."
  (handler-case
      (let ((result (load path :if-does-not-exist nil)))
        (declare (ignore result))
        ;; The file should have set *tui-loaded-theme* or we eval the last form
        ;; For simplicity, we use a convention: the file should call (in-package :hactar)
        ;; and the last form should return a tui-theme. We load and eval.
        (let ((content (uiop:read-file-string path)))
          (let ((*package* (find-package :hactar)))
            (let ((form (read-from-string content)))
              (let ((theme (eval form)))
                (when (tui-theme-p theme)
                  theme))))))
    (error (e)
      (format *error-output* "~&Error loading theme from ~A: ~A~%" path e)
      nil)))

(defun get-or-create-theme ()
  "Get the current theme, creating the default if none is set."
  (or *tui-theme*
      (if *tui-theme-name*
          (or (find-theme-by-name *tui-theme-name*)
              (make-default-theme))
          (make-default-theme))))

;;; ============================================================
;;; Sidebar Widget System
;;; ============================================================

(defstruct tui-sidebar-widget
  (name "unnamed" :type string)
  (render-fn nil :type (or null function))
  (visible-fn nil :type (or null function)))

(defvar *tui-sidebar-widgets* '()
  "Ordered list of tui-sidebar-widget structs. Rendered top to bottom.")

;;; --- Widget render functions ---
;;; Each returns the number of rows consumed.

(defun sidebar-render-project-header (win col row width max-height)
  "Render the project name and path. Returns rows used."
  (declare (ignore max-height))
  (let ((y row))
    ;; Project name
    (charms/ll:attron (charms/ll:color-pair (tui-color-pair :sidebar-title)))
    (charms/ll:attron charms/ll:a_bold)
    (tui-safe-write win (or *tui-project-name* "") col y width)
    (charms/ll:attroff charms/ll:a_bold)
    (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :sidebar-title)))
    (incf y)
    ;; Project path (muted)
    (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
    (tui-safe-write win (str:shorten width *tui-project-path* :ellipsis "..") col y width)
    (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
    (incf y)
    (- y row)))

(defun sidebar-render-model (win col row width max-height)
  "Render the model section. Returns rows used."
  (let ((y row))
    (when (<= (- y row) max-height)
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (tui-safe-write win "Model" col y width)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (incf y))
    (when (<= (- y row) max-height)
      (tui-safe-write win (format nil "  ~A" (or *tui-sidebar-model* "None")) col y width)
      (incf y))
    (when (and *tui-thinking-seconds* (<= (- y row) max-height))
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
      (tui-safe-write win (format nil "  Thinking ~As" *tui-thinking-seconds*) col y width)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
      (incf y))
    (- y row)))

(defun sidebar-render-files (win col row width max-height)
  "Render the files-in-context section. Returns rows used."
  (let ((y row))
    (when (<= (- y row) max-height)
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (tui-safe-write win "Files in Context" col y width)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (incf y))
    (if *tui-sidebar-files*
        (let* ((avail (- max-height (- y row)))
               (files-to-show (min (length *tui-sidebar-files*) (max 1 avail)))
               (remaining (- (length *tui-sidebar-files*) files-to-show)))
          (loop for f in *tui-sidebar-files*
                for i from 0 below files-to-show
                while (<= (- y row) max-height)
                do (tui-safe-write win (str:shorten width (format nil "  ~A" f) :ellipsis "..")
                                   col y width)
                   (incf y))
          (when (and (> remaining 0) (<= (- y row) max-height))
            (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
            (tui-safe-write win (format nil "  ...and ~A more" remaining) col y width)
            (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
            (incf y)))
        (when (<= (- y row) max-height)
          (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
          (tui-safe-write win "  None" col y width)
          (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
          (incf y)))
    (- y row)))

(defun sidebar-render-tool-calls (win col row width max-height)
  "Render the tool calls section. Returns rows used."
  (let ((y row))
    (when (<= (- y row) max-height)
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (tui-safe-write win "Tool Calls" col y width)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (incf y))
    (if *tui-sidebar-tool-calls*
        (dolist (tc (reverse *tui-sidebar-tool-calls*))
          (when (<= (- y row) max-height)
            (let* ((status (getf tc :status))
                   (name (getf tc :name))
                   (icon (tui-tool-call-status-icon status))
                   (color-role (tui-tool-call-status-color-role status))
                   (display (str:shorten width
                                         (format nil "  ~A ~A" icon name)
                                         :ellipsis "..")))
              (charms/ll:attron (charms/ll:color-pair (tui-color-pair color-role)))
              (tui-safe-write win display col y width)
              (charms/ll:attroff (charms/ll:color-pair (tui-color-pair color-role)))
              (incf y))))
        (when (<= (- y row) max-height)
          (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
          (tui-safe-write win "  None" col y width)
          (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
          (incf y)))
    (- y row)))

(defun sidebar-render-lsps (win col row width max-height)
  "Render the LSPs section. Returns rows used."
  (let ((y row))
    (when (<= (- y row) max-height)
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (tui-safe-write win "LSPs" col y width)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (incf y))
    (if *tui-sidebar-lsps*
        (dolist (lsp *tui-sidebar-lsps*)
          (when (<= (- y row) max-height)
            (charms/ll:attron (charms/ll:color-pair (tui-color-pair :lsp-ok)))
            (tui-safe-write win "*" col y 1)
            (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :lsp-ok)))
            (tui-safe-write win (format nil " ~A" (cdr (assoc :name lsp)))
                            (+ col 2) y (- width 2))
            (incf y)))
        (when (<= (- y row) max-height)
          (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
          (tui-safe-write win "  None" col y width)
          (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
          (incf y)))
    (- y row)))

(defun sidebar-render-mcps (win col row width max-height)
  "Render the MCPs section. Returns rows used."
  (let ((y row))
    (when (<= (- y row) max-height)
      (charms/ll:attron (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (tui-safe-write win "MCPs" col y width)
      (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :sidebar-label)))
      (incf y))
    (if *tui-sidebar-mcps*
        (dolist (mcp *tui-sidebar-mcps*)
          (when (<= (- y row) max-height)
            (tui-safe-write win (format nil "  ~A" mcp) col y width)
            (incf y)))
        (when (<= (- y row) max-height)
          (charms/ll:attron (charms/ll:color-pair (tui-color-pair :muted)))
          (tui-safe-write win "  None" col y width)
          (charms/ll:attroff (charms/ll:color-pair (tui-color-pair :muted)))
          (incf y)))
    (- y row)))

;;; --- Widget factory functions ---

(defun make-sidebar-widget-project-header ()
  "Create the project header sidebar widget."
  (make-tui-sidebar-widget
   :name "project-header"
   :render-fn #'sidebar-render-project-header))

(defun make-sidebar-widget-model ()
  "Create the model info sidebar widget."
  (make-tui-sidebar-widget
   :name "model"
   :render-fn #'sidebar-render-model))

(defun make-sidebar-widget-files ()
  "Create the files-in-context sidebar widget."
  (make-tui-sidebar-widget
   :name "files"
   :render-fn #'sidebar-render-files))

(defun make-sidebar-widget-tool-calls ()
  "Create the tool calls sidebar widget."
  (make-tui-sidebar-widget
   :name "tool-calls"
   :render-fn #'sidebar-render-tool-calls))

(defun make-sidebar-widget-lsps ()
  "Create the LSPs sidebar widget."
  (make-tui-sidebar-widget
   :name "lsps"
   :render-fn #'sidebar-render-lsps))

(defun make-sidebar-widget-mcps ()
  "Create the MCPs sidebar widget."
  (make-tui-sidebar-widget
   :name "mcps"
   :render-fn #'sidebar-render-mcps))

(defun make-default-sidebar-widgets ()
  "Create the default sidebar widget list."
  (list (make-sidebar-widget-project-header)
        (make-sidebar-widget-model)
        (make-sidebar-widget-files)
        (make-sidebar-widget-tool-calls)
        (make-sidebar-widget-lsps)
        (make-sidebar-widget-mcps)))

;;; ============================================================
;;; Tool call status → role mapping (replaces color-pair ID returns)
;;; ============================================================

(defun tui-tool-call-status-color-role (status)
  "Return the theme role keyword for a tool call status string."
  (cond
    ((string= status "pending") :tool-pending)
    ((string= status "in_progress") :tool-running)
    ((string= status "completed") :tool-done)
    ((string= status "failed") :tool-failed)
    (t :muted)))
