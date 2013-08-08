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

(in-package :Lunette.Examples.Coordinates-Origin)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (SelectObject hdc (GetStockObject BLACK_BRUSH))

             (with-foreign-object (aRect 'RECT)
                                  (GetClientRect hWnd aRect)

                                  (let ((width (- (foreign-slot-value aRect 'RECT 'right)
                                                  (foreign-slot-value aRect 'RECT 'left)))
                                        (height (- (foreign-slot-value aRect 'RECT 'bottom)
                                                   (foreign-slot-value aRect 'RECT 'top))))
                                    (SetWindowOrgEx hdc
                                                    (floor (- (/ width 2)))
                                                    (floor (- (/ height 2)))
                                                    (null-pointer))))

             (MoveToEx hdc -100 100 (null-pointer))
             (LineTo hdc 100 -100)

             (MoveToEx hdc 100 100 (null-pointer))
             (LineTo hdc -100 -100))

    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "CoorOriWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "CoorOriWin" "Coordinates: Origin")))
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
