;; WB-tree File Based Associative String Data Base System.
;; Copyright (C) 1991, 1992, 1993, 2000, 2010 Free Software Foundation, Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(require 'rev2-procedures)

(defmacro:load (in-vicinity (program-vicinity) "wbsys.scm"))

(slib:load-source (in-vicinity (program-vicinity) "blink"))
(slib:load-source (in-vicinity (program-vicinity) "ents"))
(slib:load-source (in-vicinity (program-vicinity) "handle"))
(slib:load-source (in-vicinity (program-vicinity) "segs"))
(slib:load-source (in-vicinity (program-vicinity) "prev"))
(slib:load-source (in-vicinity (program-vicinity) "del"))
(slib:load-source (in-vicinity (program-vicinity) "scan"))
(slib:load-source (in-vicinity (program-vicinity) "stats"))
(slib:load-source (in-vicinity (program-vicinity) "blkio"))

(slib:load-source (in-vicinity (program-vicinity) "db"))
(provide 'wb)

;;(trace-all "scan.scm" "blink.scm" "ents.scm" "handle.scm" "segs.scm" "prev.scm" "del.scm") (untrace long2str! str2long str2long str2short short2str! long2str! set-field split-key-pos) (set! *qp-width* 333)
