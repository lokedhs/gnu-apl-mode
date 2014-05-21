((c++-mode . ((eval . (progn
                        (em-append-include-dirs (list (expand-file-name "~/src/apl/src/native")))
                        (when (eq system-type 'darwin)
                          (em-append-include-dirs (list "/usr/local/Cellar/gettext/0.18.3.2/include")))
                        (flycheck-mode 1)
                        (company-mode 1))))))
