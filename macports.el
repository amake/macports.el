;;; macports.el --- A porcelain for MacPorts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A porcelain for MacPorts

;;; Code:

(require 'transient)
(require 'macports-core)
(require 'macports-outdated)
(require 'macports-installed)
(require 'macports-select)

(provide 'macports)
;;; macports.el ends here
