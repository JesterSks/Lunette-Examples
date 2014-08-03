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

(defpackage :Lunette.Examples.Pen-Styles
  (:use #:common-lisp
        #:cffi
        #:cffi-sys
        #:Lunette)
  (:export #:winmain))

(defpackage :Lunette.Examples.Brush-Styles
  (:use #:common-lisp
        #:cffi
        #:cffi-sys
        #:Lunette)
  (:export #:winmain))

(defpackage :Lunette.Examples.Filled-Shapes
  (:use #:common-lisp
        #:cffi
        #:cffi-sys
        #:Lunette)
  (:export #:winmain))

(defpackage :Lunette.Examples.Lines
  (:use #:common-lisp
        #:cffi
        #:cffi-sys
        #:Lunette)
  (:export #:winmain))

(defpackage :Lunette.Examples.Poly-Winding
  (:use #:common-lisp
        #:cffi
        #:cffi-sys
        #:Lunette)
  (:export #:winmain))

(defpackage :Lunette.Examples.Drawing-Modes
  (:use #:common-lisp
        #:cffi
        #:cffi-sys
        #:Lunette)
  (:export #:winmain))

(defpackage :Lunette.Examples.Coordinates-Origin
  (:use #:common-lisp
        #:cffi
        #:cffi-sys
        #:Lunette)
  (:export #:winmain))

(defpackage :Lunette.Examples.Mapping-Mode
  (:use #:common-lisp
        #:cffi
        #:cffi-sys
        #:Lunette)
  (:export #:winmain))
