(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/12:/opt/homebrew/opt/libgccjit/lib/gcc/12:/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin22/12")

;; Minimal UI
(setq-default
 package-native-compile t
 default-frame-alist
 '((tool-bar-lines . 0)
   (menu-bar-lines . 0)
;;   (undecorated . t)
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)))
