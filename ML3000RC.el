(eval-after-load "deft" '(setq deft-directory (concat (getenv "HOME") "/Dropbox/deft notes/")))
(eval-after-load "markdown" '(setq markdown-command "perl d:/view_store/z002w5en/bin/markdown"))
(eval-after-load "locate" '(setq locate-command "es"))

(setenv "GRADLE_OPTS" "-Dhttp.proxyHost=165.226.204.104 -Dhttp.proxyPort=8080 -Dhttp.nonProxyHosts=ml1002pc -Dorg.gradle.daemon=false -Xmx1500m -XX:MaxPermSize=1500m")
