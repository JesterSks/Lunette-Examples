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

(in-package :Lunette.Examples.Lines)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (let ((pts #((0 0)
                          (25 100)
                          (50 50)
                          (100 50)
                          (100 0))))

               (let ((startX 20)
                     (startY 20))
                 (MoveToEx hdc startX startY (null-pointer))
                 (loop for i
                       across pts
                       do (let* ((x (+ startX (car i)))
                                 (y (+ startY (cadr i))))
                            (LineTo hdc x y)))
                 (LineTo hdc (+ startX 50) startY))

               (let ((startX 20)
                     (startY 135))
                 (MoveToEx hdc startX startY (null-pointer))
                 (with-foreign-object (points 'POINT 5)
                                      (loop for i
                                            from 0 to 4
                                            do (let* ((aPoint (mem-aref points 'POINT i))
                                                      (x (+ startX (car (aref pts i))))
                                                      (y (+ startY (cadr (aref pts i)))))
                                                 (setf (foreign-slot-value aPoint 'POINT 'x) x
                                                       (foreign-slot-value aPoint 'POINT 'y) y)))
                                      (Polyline hdc points 5))
                 (LineTo hdc (+ startX 50) startY))

               (let ((startX 20)
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
                                      (PolylineTo hdc points 5))
                 (LineTo hdc (+ startX 50) startY)))

             (let ((startX 20)
                   (startY 365)
                   (pts #((0 100)
                          (25 0)
                          (25 100)
                          (50 0)
                          (50 100)
                          (100 0))))
               (MoveToEx hdc startX startY (null-pointer))
               (with-foreign-objects ((points 'POINT 6)
                                      (polyPts 'DWORD 2))

                                     (loop for i
                                           from 0 to 5
                                           do (let* ((aPoint (mem-aref points 'POINT i))
                                                     (x (+ startX (car (aref pts i))))
                                                     (y (+ startY (cadr (aref pts i)))))
                                                (setf (foreign-slot-value aPoint 'POINT 'x) x
                                                      (foreign-slot-value aPoint 'POINT 'y) y)))

                                     (setf (mem-aref polyPts 'DWORD 0) 3
                                           (mem-aref polyPts 'DWORD 1) 3)

                                     (PolyPolyline hdc points polyPts 2))
               (LineTo hdc (+ startX 50) startY))

             (let ((startX 135)
                   (startY 20))
               (MoveToEx hdc startX startY (null-pointer))

               (Arc hdc startX startY (+ startX 100) (+ startY 100)
                    startX (+ startY 100) (+ startX 100) (+ startY 50))

               (LineTo hdc (+ startX 50) startY))

             (let ((startX 135)
                   (startY 135))
               (MoveToEx hdc startX startY (null-pointer))

               (ArcTo hdc startX startY (+ startX 100) (+ startY 100)
                      startX (+ startY 100) (+ startX 100) (+ startY 50))

               (LineTo hdc (+ startX 50) startY))

             (let ((startX 135)
                   (startY 250))
               (MoveToEx hdc startX startY (null-pointer))

               (AngleArc hdc (+ startX 50) (+ startY 50) 50 225.0 135.0)

               (LineTo hdc (+ startX 50) startY))

             (let ((pts #((0 0)
                          (0 100)
                          (100 0)
                          (100 50)
                          (100 100)
                          (200 100)
                          (200 0))))

               (let ((startX 250)
                     (startY 20))
                 (MoveToEx hdc startX startY (null-pointer))
                 (with-foreign-object (points 'POINT 7)
                                      (loop for i
                                            from 0 to 6
                                            do (let* ((aPoint (mem-aref points 'POINT i))
                                                      (x (+ startX (car (aref pts i))))
                                                      (y (+ startY (cadr (aref pts i)))))
                                                 (setf (foreign-slot-value aPoint 'POINT 'x) x
                                                       (foreign-slot-value aPoint 'POINT 'y) y)))
                                      (PolyBezier hdc points 7))
                 (LineTo hdc (+ startX 50) startY))

               (let ((startX 250)
                     (startY 135))
                 (MoveToEx hdc startX startY (null-pointer))
                 (with-foreign-object (points 'POINT 6)
                                      (loop for i
                                            from 1 to 6
                                            do (let* ((aPoint (mem-aref points 'POINT (1- i)))
                                                      (x (+ startX (car (aref pts i))))
                                                      (y (+ startY (cadr (aref pts i)))))
                                                 (setf (foreign-slot-value aPoint 'POINT 'x) x
                                                       (foreign-slot-value aPoint 'POINT 'y) y)))
                                      (PolyBezierTo hdc points 6))
                 (LineTo hdc (+ startX 50) startY)))

             (let ((pts #((0 0)
                          (50 50)
                          (50 100)
                          (150 100)
                          (200 0)
                          (100 50))))

               (let ((startX 250)
                     (startY 250))
                 (with-foreign-objects ((points 'POINT 6)
                                        (ptTypes 'BYTE 6))

                                       (loop for i
                                             from 0 to 5
                                             do (let* ((aPoint (mem-aref points 'POINT i))
                                                       (x (+ startX (car (aref pts i))))
                                                       (y (+ startY (cadr (aref pts i)))))
                                                  (setf (foreign-slot-value aPoint 'POINT 'x) x
                                                        (foreign-slot-value aPoint 'POINT 'y) y)))

                                       (setf (mem-aref ptTypes 'BYTE 0) PT_MOVETO
                                             (mem-aref ptTypes 'BYTE 1) PT_LINETO
                                             (mem-aref ptTypes 'BYTE 2) PT_LINETO
                                             (mem-aref ptTypes 'BYTE 3) PT_BEZIERTO
                                             (mem-aref ptTypes 'BYTE 4) PT_BEZIERTO
                                             (mem-aref ptTypes 'BYTE 5) PT_BEZIERTO)

                                       (PolyDraw hdc points ptTypes 6))
                 (LineTo hdc (+ startX 50) startY))

               (let ((startX 250)
                     (startY 365))
                 (with-foreign-objects ((points 'POINT 6)
                                        (ptTypes 'BYTE 6))

                                       (loop for i
                                             from 0 to 5
                                             do (let* ((aPoint (mem-aref points 'POINT i))
                                                       (x (+ startX (car (aref pts i))))
                                                       (y (+ startY (cadr (aref pts i)))))
                                                  (setf (foreign-slot-value aPoint 'POINT 'x) x
                                                        (foreign-slot-value aPoint 'POINT 'y) y)))

                                       (setf (mem-aref ptTypes 'BYTE 0) PT_MOVETO
                                             (mem-aref ptTypes 'BYTE 1) PT_LINETO
                                             (mem-aref ptTypes 'BYTE 2) PT_LINETO
                                             (mem-aref ptTypes 'BYTE 3) PT_BEZIERTO
                                             (mem-aref ptTypes 'BYTE 4) PT_BEZIERTO
                                             (mem-aref ptTypes 'BYTE 5) (logior PT_BEZIERTO PT_CLOSEFIGURE))

                                       (PolyDraw hdc points ptTypes 6))
                 (LineTo hdc (+ startX 50) startY))))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "LineWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "LineWin" "Lines")))
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
