(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages admin)
             (gnu packages gnuzilla)
             (guix gexp))

(home-environment
 (packages (list
            ;; Browser
            icecat)))
;; (services (list (service home-bash-service-type
;;                          (home-bash-configuration
;;                           (environment-variables '(("EDITOR" . "emacsclient")))
;;                           (aliases '(("ggs" . "git status")))))))
