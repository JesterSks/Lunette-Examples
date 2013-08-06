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

(in-package :Lunette.Examples.Drawing-Modes)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_PAINT)
    (let ((hPenDashDotDot (CreatePen PS_DASHDOTDOT 1 (RGB 0 0 0))))


      (with-ps ps hdc hWnd
               (SelectObject hdc (GetStockObject BLACK_BRUSH))

               (Rectangle hdc 120 30 220 195)

               (SelectObject hdc hPenDashDotDot)
               (MoveToEx hdc 20 20 (null-pointer))
               (LineTo hdc 320 20)

               (SetROP2 hdc R2_BLACK)
               (MoveToEx hdc 20 35 (null-pointer))
               (LineTo hdc 320 35)

               (SetROP2 hdc R2_NOTMERGEPEN)
               (MoveToEx hdc 20 45 (null-pointer))
               (LineTo hdc 320 45)

               (SetROP2 hdc R2_MASKNOTPEN)
               (MoveToEx hdc 20 55 (null-pointer))
               (LineTo hdc 320 55)

               (SetROP2 hdc R2_NOTCOPYPEN)
               (MoveToEx hdc 20 65 (null-pointer))
               (LineTo hdc 320 65)

               (SetROP2 hdc R2_MASKPENNOT)
               (MoveToEx hdc 20 75 (null-pointer))
               (LineTo hdc 320 75)

               (SetROP2 hdc R2_NOT)
               (MoveToEx hdc 20 85 (null-pointer))
               (LineTo hdc 320 85)

               (SetROP2 hdc R2_XORPEN)
               (MoveToEx hdc 20 95 (null-pointer))
               (LineTo hdc 320 95)

               (SetROP2 hdc R2_NOTMASKPEN)
               (MoveToEx hdc 20 105 (null-pointer))
               (LineTo hdc 320 105)

               (SetROP2 hdc R2_MASKPEN)
               (MoveToEx hdc 20 115 (null-pointer))
               (LineTo hdc 320 115)

               (SetROP2 hdc R2_NOTXORPEN)
               (MoveToEx hdc 20 125 (null-pointer))
               (LineTo hdc 320 125)

               (SetROP2 hdc R2_NOP)
               (MoveToEx hdc 20 135 (null-pointer))
               (LineTo hdc 320 135)

               (SetROP2 hdc R2_MERGENOTPEN)
               (MoveToEx hdc 20 145 (null-pointer))
               (LineTo hdc 320 145)

               (SetROP2 hdc R2_COPYPEN)
               (MoveToEx hdc 20 155 (null-pointer))
               (LineTo hdc 320 155)

               (SetROP2 hdc R2_MERGEPENNOT)
               (MoveToEx hdc 20 165 (null-pointer))
               (LineTo hdc 320 165)

               (SetROP2 hdc R2_MERGEPEN)
               (MoveToEx hdc 20 175 (null-pointer))
               (LineTo hdc 320 175)

               (SetROP2 hdc R2_WHITE)
               (MoveToEx hdc 20 185 (null-pointer))
               (LineTo hdc 320 185))

      (DeleteObject hPenDashDotDot))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "DrawingModesWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "DrawingModesWin" "Drawing Modes")))
    (ShowWindow hwnd SW_SHOW)
    (ShowWindow hwnd SW_SHOW)
    (UpdateWindow hwnd)

    (with-foreign-object (msg 'MSG)
                         (loop with result
                               while (not (= 0 (setf result (GetMessage MSG (null-pointer) 0 0))))
                               do (progn
                                    (TranslateMessage msg)
                                    (DispatchMessage msg)))
                         (foreign-slot-value msg 'MSG 'wParam))))
