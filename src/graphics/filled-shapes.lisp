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

(in-package :Lunette.Examples.Filled-Shapes)

(defun drawShapes (hdc)
  (let ((startX 20)
        (startY 20))
    (Rectangle hdc startX startY (+ startX 100) (+ startY 100))
    (Rectangle hdc (+ startX 115) startY (+ startX 215) (+ startY 50))
    (Rectangle hdc (+ startX 230) startY (+ startX 270) (+ startY 100)))

  (let ((startX 20)
        (startY 135))
    (Ellipse hdc startX startY (+ startX 100) (+ startY 100))
    (Ellipse hdc (+ startX 115) startY (+ startX 215) (+ startY 50))
    (Ellipse hdc (+ startX 230) startY (+ startX 270) (+ startY 100)))

  (let ((startX 20)
        (startY 250))
    (RoundRect hdc startX startY (+ startX 100) (+ startY 100) 10 10)
    (RoundRect hdc (+ startX 115) startY (+ startX 215) (+ startY 50) 10 10)
    (RoundRect hdc (+ startX 230) startY (+ startX 270) (+ startY 100) 10 10))

  (let ((startX 20)
        (startY 365))
    (RoundRect hdc startX startY (+ startX 100) (+ startY 100) 20 10)
    (RoundRect hdc (+ startX 115) startY (+ startX 215) (+ startY 50) 20 10)
    (RoundRect hdc (+ startX 230) startY (+ startX 270) (+ startY 100) 20 10))

  (let ((startX 20)
        (startY 480))
    (RoundRect hdc startX startY (+ startX 100) (+ startY 100) 10 20)
    (RoundRect hdc (+ startX 115) startY (+ startX 215) (+ startY 50) 10 20)
    (RoundRect hdc (+ startX 230) startY (+ startX 270) (+ startY 100) 10 20))

  (let ((startX 305)
        (startY 20))
    (Chord hdc startX startY (+ startX 100) (+ startY 100) (+ startX 100) (+ startY 50) startX (+ startY 100))
    (Chord hdc (+ startX 115) startY (+ startX 215) (+ startY 50) (+ startX 215) (+ startY 25) (+ startX 115) (+ startY 50))
    (Chord hdc (+ startX 230) startY (+ startX 270) (+ startY 100) (+ startX 270) (+ startY 50) (+ startX 230) (+ startY 100) ))

  (let ((startX 305)
        (startY 135))
    (Pie hdc startX startY (+ startX 100) (+ startY 100) (+ startX 100) (+ startY 50) startX (+ startY 100))
    (Pie hdc (+ startX 115) startY (+ startX 215) (+ startY 50) (+ startX 215) (+ startY 25) (+ startX 115) (+ startY 50))
    (Pie hdc (+ startX 230) startY (+ startX 270) (+ startY 100) (+ startX 270) (+ startY 50) (+ startX 230) (+ startY 100) ))

  (let ((pts #((50 0)
               (0 50)
               (25 100)
               (75 100)
               (100 50)
               (150 0)
               (125 100)
               (175 100))))

    (let ((startX 305)
          (startY 250))
      (MoveToEx hdc startX startY (null-pointer))
      (with-foreign-object (points 'POINT 5)
                           (loop for i
                                 from 0 to 4
                                 do (let* ((aPoint (mem-aref points 'POINT i))
                                           (x (+ startX (car (aref pts i))))
                                           (y (+ startY (cadr (aref pts i)))))
                                      (setf (foreign-slot-value aPoint 'POINT 'x) x
                                            (foreign-slot-value aPoint 'POINT 'y) y)))
                           (Polygon hdc points 5)))

    (let ((startX 305)
          (startY 365))
      (MoveToEx hdc startX startY (null-pointer))
      (with-foreign-objects ((points 'POINT 8)
                             (ptCount :int 2))
                            (loop for i
                                  from 0 to 7
                                  do (let* ((aPoint (mem-aref points 'POINT i))
                                            (x (+ startX (car (aref pts i))))
                                            (y (+ startY (cadr (aref pts i)))))
                                       (setf (foreign-slot-value aPoint 'POINT 'x) x
                                             (foreign-slot-value aPoint 'POINT 'y) y)))
                            (setf (mem-aref ptCount :int 0) 5
                                  (mem-aref ptCount :int 1) 3)
                            (PolyPolygon hdc points ptCount 2)))))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_PAINT)

    (with-ps ps hdc hWnd
             (SelectObject hdc (GetStockObject LTGRAY_BRUSH))
             (drawShapes hdc))

    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "FilledShapeWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "FilledShapeWin" "Filled Shapes")))
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
