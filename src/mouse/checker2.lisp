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

(in-package :Lunette.Examples.checker2)

(defconstant DIVISIONS 5)

(defparameter fState (make-array `(,DIVISIONS ,DIVISIONS) :element-type t :initial-element nil))
(defparameter cxBlock 0)
(defparameter cyBlock 0)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (with-foreign-object (rect 'RECT)
                         (GetClientRect hWnd rect)
                         (setf cxClient (- (foreign-slot-value rect 'RECT 'right)
                                           (foreign-slot-value rect 'RECT 'left))
                               cyClient (- (foreign-slot-value rect 'RECT 'bottom)
                                           (foreign-slot-value rect 'RECT 'top))))
    0)
   ((eql msg WM_SIZE)
    (setf cxBlock (floor (/ (loword lparam) DIVISIONS))
          cyBlock (floor (/ (hiword lparam) DIVISIONS)))
    0)
   ((eql msg WM_SETFOCUS)
    (ShowCursor 1)
    0)
   ((eql msg WM_KILLFOCUS)
    (ShowCursor 0)
    0)
   ((eql msg WM_KEYDOWN)
    (with-foreign-object (point 'POINT)
                         (GetCursorPos point)
                         (ScreenToClient hWnd point)

                         (let ((x (max 0 (min (1- DIVISIONS) (floor (/ (foreign-slot-value point 'POINT 'x) cxBlock)))))
                               (y (max 0 (min (1- DIVISIONS) (floor (/ (foreign-slot-value point 'POINT 'y) cyBlock))))))
                           (cond
                            ((eql wParam VK_UP)
                             (decf y))
                            ((eql wParam VK_DOWN)
                             (incf y))
                            ((eql wParam VK_LEFT)
                             (decf x))
                            ((eql wParam VK_RIGHT)
                             (incf x))
                            ((eql wParam VK_HOME)
                             (setf x 0
                                   y 0))
                            ((eql wParam VK_END)
                             (setf x (1- DIVISIONS)
                                   y (1- DIVISIONS)))
                            ((or (eql wParam VK_RETURN)
                                 (eql wParam VK_SPACE))
                             (SendMessage hWnd WM_LBUTTONDOWN MK_LBUTTON (makelong (* x cxBlock) (* y cyBlock)))))

                           (setf x (mod (+ x DIVISIONS) DIVISIONS)
                                 y (mod (+ y DIVISIONS) DIVISIONS))

                           (setf (foreign-slot-value point 'POINT 'x) (+ (* x cxBlock) (floor (/ cxBlock 2)))
                                 (foreign-slot-value point 'POINT 'y) (+ (* y cyBlock) (floor (/ cyBlock 2))))

                           (ClientToScreen hWnd point)
                           (SetCursorPos (foreign-slot-value point 'POINT 'x)
                                         (foreign-slot-value point 'POINT 'y))))
    0)
   ((eql msg WM_LBUTTONDOWN)
    (let ((x (floor (/ (loword lparam) cxBlock)))
          (y (floor (/ (hiword lparam) cyBlock))))
      (if (and (< x DIVISIONS)
               (< y DIVISIONS))
          (with-foreign-object (rect 'RECT)
                               (setf (aref fState x y) (not (aref fState x y))

                                     (foreign-slot-value rect 'RECT 'left)   (* x cxBlock)
                                     (foreign-slot-value rect 'RECT 'top)    (* y cyBlock)
                                     (foreign-slot-value rect 'RECT 'right)  (* (1+ x) cxBlock)
                                     (foreign-slot-value rect 'RECT 'bottom) (* (1+ y) cyBlock))

                               (InvalidateRect hWnd rect 0))
        (MessageBeep MB_OK)))
    0)
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (loop for x
                   from 0
                   to (1- DIVISIONS)
                   do (loop for y
                            from 0
                            to (1- DIVISIONS)
                            do (let ((left   (* x cxBlock))
                                     (top    (* y cyBlock))
                                     (right  (* (1+ x) cxBlock))
                                     (bottom (* (1+ y) cyBlock)))
                                 (Rectangle hdc left top right bottom)

                                 (when (aref fState x y)
                                   (MoveToEx hdc left top (null-pointer))
                                   (LineTo hdc right bottom)

                                   (MoveToEx hdc left bottom (null-pointer))
                                   (LineTo hdc right top))))))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "Checker2Win" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "Checker2Win" "Checker2 Mouse Hit-Test Demo")))
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
