;;;(setq foo "hello")

(find-file "project.el")
(find-file "thimbl.lisp")


;(setq inferior-lisp-program "clisp -M ./lispinit.mem")

(setq inferior-lisp-program "clisp -I -repl -i ~/.clisprc.lisp thimbl.lisp")

(split-window-vertically)
(other-window 1)
(run-lisp inferior-lisp-program )
