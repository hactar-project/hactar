(in-package :hactar)

(defprompt diffusion-system-prompt system
  "System prompt for code intelligence"
  :template "You are a code intelligence system that generates Common Lisp S-expressions to describe code structure. Output ONLY valid LISP forms. Use these forms:

   (defroutes (:file \"path\" :path \"/route\" :component \"Name\" :method :get) ...)
   (defmodules (:module \"name\" :file \"path\" :exports (...)) ...)  
   (defscripts (:script \"name\" :command \"cmd\") ...)
   (defcomponents (:component \"Name\" :file \"path\" :props (...)) ...)
   (defdeps (:dep \"name\" :version \"x.y.z\" :type :dev) ...)
   (defimports (:file \"path\" :imports ((:from \"mod\" :names (\"a\" \"b\")))) ...)
   
   Be precise with file paths and line numbers. Use :line-start and :line-end for source locations. Output nothing except valid LISP forms.")

(defprompt diffusion-routes template
  "Template for extracting routes"
  :template "Analyze these ~A project files and generate defroutes forms.

Files:
~{~A~%~}

Source contents:
~{~A~%---~%~}

Generate ONLY (defroutes ...) forms with accurate file paths and route paths.")

(defprompt diffusion-modules template
  "Template for modules"
  :template "Given this file tree and export summary, generate defmodules forms.

File tree:
~A

Exports:
~A

Generate ONLY (defmodules ...) forms.")

(defprompt diffusion-incremental-update template
  "Template for incremental update"
  :template "The following LISP forms describe the current project state:

~A

These files have changed:
~{~A~%~}

Changes:
~A

Generate UPDATED LISP forms reflecting the changes. Output complete replacement forms for any sections affected by the changes.")

(defprompt diffusion-models template
  "Template for models"
  :template "Given this Prisma schema file, generate defmodels forms.

Schema:
~A

Generate ONLY (defmodels ...) forms.")
