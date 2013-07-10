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

(in-package :Lunette.Examples.Poly-Winding)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_PAINT)

    (with-ps ps hdc hWnd
             (SelectObject hdc (GetStockObject LTGRAY_BRUSH))

             (let ((pts #((50 0)
                          (0 50)
                          (50 100)
                          (100 50)
                          (50 0)

                          (100 0)
                          (50 50)
                          (100 100)
                          (150 50)
                          (100 0)

                          (150 0)
                          (200 50)
                          (150 100)
                          (100 50)
                          (150 0))))

               (let ((startX 20)
                     (startY 20))
                 (with-foreign-objects ((points 'POINT 15))
                                       (loop for i
                                             from 0 to 14
                                             do (let* ((aPoint (mem-aref points 'POINT i))
                                                       (x (+ startX (car (aref pts i))))
                                                       (y (+ startY (cadr (aref pts i)))))
                                                  (setf (foreign-slot-value aPoint 'POINT 'x) x
                                                        (foreign-slot-value aPoint 'POINT 'y) y)))
                                       (SetPolyFillMode hdc ALTERNATE)
                                       (Polygon hdc points 15)))

               (let ((startX 20)
                     (startY 135))
                 (with-foreign-objects ((points 'POINT 15))
                                       (loop for i
                                             from 0 to 14
                                             do (let* ((aPoint (mem-aref points 'POINT i))
                                                       (x (+ startX (car (aref pts i))))
                                                       (y (+ startY (cadr (aref pts i)))))
                                                  (setf (foreign-slot-value aPoint 'POINT 'x) x
                                                        (foreign-slot-value aPoint 'POINT 'y) y)))
                                       (SetPolyFillMode hdc WINDING)
                                       (Polygon hdc points 15)))))

    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "PolyWindingWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "PolyWindingWin" "Polygon Winding")))
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
