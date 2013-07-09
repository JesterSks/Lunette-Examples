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
        #:Lunette)
  (:import-from :Lunette.Graphics
                #:PS_SOLID
                #:PS_DASH
                #:PS_DOT
                #:PS_DASHDOT
                #:PS_DASHDOTDOT
                #:PS_NULL

                #:CreatePen
                #:SelectObject
                #:DeleteObject
                #:RGB
                #:MoveToEx
                #:LineTo)
  (:export #:winmain))

(defpackage :Lunette.Examples.Brush-Styles
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.Graphics
                #:HS_HORIZONTAL
                #:HS_VERTICAL
                #:HS_FDIAGONAL
                #:HS_BDIAGONAL
                #:HS_CROSS
                #:HS_DIAGCROSS

                #:CreateSolidBrush
                #:CreateHatchBrush
                #:SelectObject
                #:DeleteObject
                #:RGB
                #:MoveToEx
                #:Rectangle)
  (:export #:winmain))

(defpackage :Lunette.Examples.Filled-Shapes
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.Graphics
                #:LTGRAY_BRUSH

                #:GetStockObject
                #:SelectObject
                #:MoveToEx
                #:Rectangle
                #:Ellipse
                #:RoundRect
                #:Arc
                #:Pie
                #:Chord)
  (:export #:winmain))

(defpackage :Lunette.Examples.Lines
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.System
                #:DWORD
                #:BYTE

                #:POINT
                #:x
                #:y)
  (:import-from :Lunette.Graphics
                #:PT_CLOSEFIGURE
                #:PT_LINETO
                #:PT_BEZIERTO
                #:PT_MOVETO

                #:MoveToEx
                #:LineTo
                #:Polyline
                #:PolylineTo
                #:PolyPolyline
                #:Arc
                #:AngleArc
                #:ArcTo
                #:PolyBezier
                #:PolyBezierTo
                #:PolyDraw)
  (:export #:winmain))
