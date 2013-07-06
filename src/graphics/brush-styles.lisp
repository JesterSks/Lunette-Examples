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

(in-package :Lunette.Examples.Brush-Styles)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_PAINT)
    (let ((hBrushPurple     (CreateSolidBrush (RGB 255 0 255)))
          (hBrushHorizontal (CreateHatchBrush HS_HORIZONTAL (RGB 0 0 0)))
          (hBrushVertical   (CreateHatchBrush HS_VERTICAL   (RGB 0 0 0)))
          (hBrushFDiagonal  (CreateHatchBrush HS_FDIAGONAL  (RGB 0 0 0)))
          (hBrushBDiagonal  (CreateHatchBrush HS_BDIAGONAL  (RGB 0 0 0)))
          (hBrushCross      (CreateHatchBrush HS_CROSS      (RGB 0 0 0)))
          (hBrushDiagCross  (CreateHatchBrush HS_DIAGCROSS  (RGB 0 0 0))))

      (with-ps ps hdc hWnd
               (SelectObject hdc hBrushPurple)
               (Rectangle hdc 20 20 220 40)

               (SelectObject hdc hBrushHorizontal)
               (Rectangle hdc 20 55 220 75)

               (SelectObject hdc hBrushVertical)
               (Rectangle hdc 20 90 220 110)

               (SelectObject hdc hBrushFDiagonal)
               (Rectangle hdc 20 125 220 145)

               (SelectObject hdc hBrushBDiagonal)
               (Rectangle hdc 235 20 435 40)

               (SelectObject hdc hBrushCross)
               (Rectangle hdc 235 55 435 75)

               (SelectObject hdc hBrushDiagCross)
               (Rectangle hdc 235 90 435 110))

      (DeleteObject hBrushPurple)
      (DeleteObject hBrushHorizontal)
      (DeleteObject hBrushVertical)
      (DeleteObject hBrushFDiagonal)
      (DeleteObject hBrushBDiagonal)
      (DeleteObject hBrushCross)
      (DeleteObject hBrushDiagCross))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "BrushStyleWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "BrushStyleWin" "Brush Styles")))
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
