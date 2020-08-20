;; -*- no-byte-compile: t; -*-
;;; input/chinese/packages.el

(package! pyim :pin "e54153f462dd8cd8e9bb379e0483a2849ec94f42")
(package! ace-pinyin :pin "8b2e9335b02486730ea4ceee790130cc5328f9ea")
(package! pangu-spacing :pin "f92898949ba3bf991fd229416f3bbb54e9c6c223")

(package! smart-input-source
  :recipe (:host github :repo "laishulu/emacs-smart-input-source"))
