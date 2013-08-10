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

(in-package :Lunette.Examples.Mapping-Mode)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (SelectObject hdc (GetStockObject BLACK_BRUSH))
             (SetMapMode hdc MM_LOENGLISH)

             (with-foreign-object (aRect 'RECT)
                                  (GetClientRect hWnd aRect)

                                  (let* ((width (- (foreign-slot-value aRect 'RECT 'right)
                                                   (foreign-slot-value aRect 'RECT 'left)))
                                         (height (- (foreign-slot-value aRect 'RECT 'bottom)
                                                    (foreign-slot-value aRect 'RECT 'top)))
                                         (halfw (ceiling (/ width 2)))
                                         (halfh (ceiling (/ height 2))))
                                    (SetViewportOrgEx hdc halfw halfh (null-pointer))

                                    (MoveToEx hdc (- halfw) 0 (null-pointer))
                                    (LineTo hdc halfw 0)

                                    (MoveToEx hdc 0 100 (null-pointer))
                                    (LineTo hdc 0 -100)

                                    (loop for x
                                          from 100
                                          to halfw by 100
                                          do (progn
                                               (MoveToEx hdc x 20 (null-pointer))
                                               (LineTo hdc x -20)

                                               (MoveToEx hdc (- x) 20 (null-pointer))
                                               (LineTo hdc (- x) -20)))

                                    (loop for x
                                          from 50
                                          to halfw by 100
                                          do (progn
                                               (MoveToEx hdc x 10 (null-pointer))
                                               (LineTo hdc x -10)

                                               (MoveToEx hdc (- x) 10 (null-pointer))
                                               (LineTo hdc (- x) -10)
                                               )))))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "MappingModeWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "MappingModeWin" "Coordinates: Mapping Mode")))
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
