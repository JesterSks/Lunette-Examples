#|
   Copyright 2013 Robert Burghart

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

(defpackage #:lunette-examples-system
  (:use #:common-lisp #:asdf))

(in-package #:lunette-examples-system)

(defsystem lunette-examples
  :version "0.0.1"
  :description "Code examples using the CFFI wrappers for 32-bit Win32 (R) functions."
  :author "Robert Burghart <JesterSks@gmail.com>"
  :maintainer "Robert Burghart <JesterSks@gmail.com>"
  :license "Apache License Version 2.0"
  :depends-on (:cffi
               :cl-opengl
               :lunette
               #-swank
               :swank)
  :components
  ((:module "src/simple"
            :components
            ((:file "package")
             (:file "simple-win"  :depends-on ("package"))
             (:file "hello-paint" :depends-on ("package"))))
   (:module "src/graphics"
            :components
            ((:file "package")
             (:file "pen-styles"         :depends-on ("package"))
             (:file "brush-styles"       :depends-on ("package"))
             (:file "filled-shapes"      :depends-on ("package"))
             (:file "lines"              :depends-on ("package"))
             (:file "poly-winding"       :depends-on ("package"))
             (:file "drawing-modes"      :depends-on ("package"))
             (:file "coordinates-origin" :depends-on ("package"))
             (:file "mapping-mode"       :depends-on ("package"))))
   (:module "src/keyboard"
            :components
            ((:file "package")
             (:file "typer"       :depends-on ("package"))
             (:file "char"        :depends-on ("package"))
             (:file "virtual-key" :depends-on ("package"))
             (:file "textout"     :depends-on ("package"))
             (:file "textout2"    :depends-on ("package"))
             (:file "textout3"    :depends-on ("package"))))
   (:module "src/mouse"
            :components
            ((:file "package")
             (:file "connect"  :depends-on ("package"))
             (:file "checker1" :depends-on ("package"))
             (:file "checker2" :depends-on ("package"))
             (:file "checker3" :depends-on ("package"))
             (:file "checker4" :depends-on ("package"))
             (:file "blokout1" :depends-on ("package"))
             (:file "blokout2" :depends-on ("package"))
             (:file "click"    :depends-on ("package"))
             (:file "click2"   :depends-on ("package"))))
   (:module "src/controls"
            :components
            ((:file "package")
             (:file "btnlook"  :depends-on ("package"))
             (:file "btnlook1" :depends-on ("package"))
             (:file "owndraw"  :depends-on ("package"))
             (:file "colors1"  :depends-on ("package"))
             (:file "environ"  :depends-on ("package"))
             (:file "head"     :depends-on ("package"))))
   (:module "src/poppad"
            :components
            ((:file "package")
             (:file "poppad1" :depends-on ("package"))))
   (:module "src/swank"
            :components
            ((:file "package")
             (:file "swank1" :depends-on ("package"))))
   (:module "src/wgl"
            :components
            ((:file "package")
             (:file "wgl-simple" :depends-on ("package"))))))
