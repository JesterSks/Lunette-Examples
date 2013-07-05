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

(in-package :Lunette.Examples.Pen-Styles)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_PAINT)
    (let ((hPenSolid      (CreatePen PS_SOLID      1 (RGB 0 0 0)))
          (hPenDash       (CreatePen PS_DASH       1 (RGB 0 0 0)))
          (hPenDot        (CreatePen PS_DOT        1 (RGB 0 0 0)))
          (hPenDashDot    (CreatePen PS_DASHDOT    1 (RGB 0 0 0)))
          (hPenDashDotDot (CreatePen PS_DASHDOTDOT 1 (RGB 0 0 0)))
          (hPenNull       (CreatePen PS_NULL       1 (RGB 0 0 0))))

      (with-ps ps hdc hWnd
               (SelectObject hdc hPenSolid)
               (MoveToEx hdc 20 20 (null-pointer))
               (LineTo hdc 220 20)

               (SelectObject hdc hPenDash)
               (MoveToEx hdc 20 35 (null-pointer))
               (LineTo hdc 220 35)

               (SelectObject hdc hPenDot)
               (MoveToEx hdc 20 50 (null-pointer))
               (LineTo hdc 220 50)

               (SelectObject hdc hPenDashDot)
               (MoveToEx hdc 20 65 (null-pointer))
               (LineTo hdc 220 65)

               (SelectObject hdc hPenDashDotDot)
               (MoveToEx hdc 20 80 (null-pointer))
               (LineTo hdc 220 80)

               (SelectObject hdc hPenNull)
               (MoveToEx hdc 20 95 (null-pointer))
               (LineTo hdc 220 95))

      (DeleteObject hPenSolid)
      (DeleteObject hPenDash)
      (DeleteObject hPenDot)
      (DeleteObject hPenDashDot)
      (DeleteObject hPenDashDotDot)
      (DeleteObject hPenNull))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "PenStyleWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "PenStyleWin" "Pen Styles")))
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
