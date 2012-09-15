;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/cedet-eieio-settings.el
;; Time-stamp: <2011-02-05 15:18>

;; This  file is free  software; you  can redistribute  it and/or
;; modify it under the terms of the GNU General Public License as
;; published by  the Free Software Foundation;  either version 3,
;; or (at your option) any later version.

;; This file is  distributed in the hope that  it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You  should have  received a  copy of  the GNU  General Public
;; License along with  GNU Emacs; see the file  COPYING.  If not,
;; write  to  the Free  Software  Foundation,  Inc., 51  Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.


(require 'eieio-face-settings)


(defun eieio-settings ()
  "Settings for `eieio'.")


(defun chart-settings ()
  "Settings for `chart'."
  (eal-define-keys
   'chart-map
   `(("q"   delete-current-window)
     ("'"   switch-to-other-buffer)
     ("u"   View-scroll-half-page-backward)
     ("SPC" scroll-up)
     ("."   find-symbol-at-point)
     ("1"   delete-other-windows))))

(eval-after-load "chart"
  `(chart-settings))

(eval-after-load "eieio"
  `(eieio-settings))

(provide 'eieio-settings)
