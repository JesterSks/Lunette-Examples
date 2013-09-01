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

(in-package :Lunette.Examples.checker4)

(defconstant DIVISIONS 5)

(defparameter hwndChild (make-array `(,DIVISIONS ,DIVISIONS) :element-type t :initial-element nil))
(defparameter idFocus 0)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (let ((cxBlock 0)
          (cyBlock 0))
      (with-foreign-object (rect 'RECT)
                           (GetClientRect hWnd rect)
                           (setf cxClient (- (foreign-slot-value rect 'RECT 'right)
                                             (foreign-slot-value rect 'RECT 'left))
                                 cyClient (- (foreign-slot-value rect 'RECT 'bottom)
                                             (foreign-slot-value rect 'RECT 'top))))
      (loop for x from 0 to (1- DIVISIONS)
            do (loop for y from 0 to (1- DIVISIONS)
                     do (setf (aref hwndChild x y)
                              (create-window-ex "Checker4_Child" ""
                                                :dwStyle (logior WS_CHILDWINDOW WS_VISIBLE)
                                                :x (* x cxBlock)
                                                :y (* y cyBlock)
                                                :nWidth cxBlock
                                                :nHeight cyBlock
                                                :hWndParent hwnd
                                                :hMenu (make-pointer (logior x (ash y 8))))))))
    0)
   ((eql msg WM_SIZE)
    (let ((cxBlock (floor (/ (loword lparam) DIVISIONS)))
          (cyBlock (floor (/ (hiword lparam) DIVISIONS))))
      (loop for x from 0 to (1- DIVISIONS)
            do (loop for y from 0 to (1- DIVISIONS)
                     do (MoveWindow (aref hwndChild x y)
                                    (* x cxBlock)
                                    (* y cyBlock)
                                    cxBlock
                                    cyBlock
                                    1))))
    0)
   ((eql msg WM_SETFOCUS)
    (SetFocus (GetDlgItem hWnd idFocus))
    0)
   ((eql msg WM_KEYDOWN)
    (let ((x (logand idFocus #xff))
          (y (ash idFocus -8)))

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
              y (1- DIVISIONS))))

      (setf x (mod (+ x DIVISIONS) DIVISIONS)
            y (mod (+ y DIVISIONS) DIVISIONS))

      (setf idFocus (logior x (ash y 8)))

      (SetFocus (GetDlgItem hWnd idFocus)))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defcallback ChildWndProc LRESULT ((hWnd HWND)
                                   (msg :UINT)
                                   (wparam WPARAM)
                                   (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (SetWindowLong hWnd 0 0)
    0)
   ((eql msg WM_KEYDOWN)
    (if (and (not (= wParam VK_RETURN))
             (not (= wParam VK_SPACE)))
      (SendMessage (GetParent hWnd) msg wParam lParam)
      (progn
        (SetWindowLong hWnd 0 (logxor 1 (GetWindowLong hWnd 0)))
        (SetFocus hWnd)
        (InvalidateRect hWnd (null-pointer) 0)))
    0)
   ((eql msg WM_LBUTTONDOWN)
    (SetWindowLong hWnd 0 (logxor 1 (GetWindowLong hWnd 0)))
    (SetFocus hWnd)
    (InvalidateRect hWnd (null-pointer) 0)
    0)
   ((eql msg WM_SETFOCUS)
    (setf idFocus (GetWindowLong hWnd GWL_ID))
    (InvalidateRect hWnd (null-pointer) 1)
    0)
   ((eql msg WM_KILLFOCUS)
    (InvalidateRect hWnd (null-pointer) 1)
    0)
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (with-foreign-object (rect 'RECT)
                                  (GetClientRect hWnd rect)

                                  (let ((right (foreign-slot-value rect 'RECT 'right))
                                        (bottom (foreign-slot-value rect 'RECT 'bottom)))

                                    (Rectangle hdc 0 0 right bottom)

                                    (when (not (= 0 (GetWindowLong hWnd 0)))
                                      (MoveToEx hdc 0 0 (null-pointer))
                                      (LineTo hdc right bottom)

                                      (MoveToEx hdc 0 bottom (null-pointer))
                                      (LineTo hdc right 0))

                                    (when (eql hWnd (GetFocus))
                                      (incf (foreign-slot-value rect 'RECT 'left) (floor (/ right 10)))
                                      (decf (foreign-slot-value rect 'RECT 'right)
                                            (foreign-slot-value rect 'RECT 'left))
                                      (incf (foreign-slot-value rect 'RECT 'top) (floor (/ bottom 10)))
                                      (decf (foreign-slot-value rect 'RECT 'bottom)
                                            (foreign-slot-value rect 'RECT 'top))

                                      (SelectObject hdc (GetStockObject NULL_BRUSH))
                                      (SelectObject hdc (CreatePen PS_DASH 0 0))
                                      (Rectangle hdc (foreign-slot-value rect 'RECT 'left)
                                                 (foreign-slot-value rect 'RECT 'top)
                                                 (foreign-slot-value rect 'RECT 'right)
                                                 (foreign-slot-value rect 'RECT 'bottom))
                                      (DeleteObject (SelectObject hdc (GetStockObject BLACK_PEN)))))))
    0)
   (t (DefWindowProc hWnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "Checker4_Child" :lpfnWndProc (callback ChildWndProc)
                  :hIcon (null-pointer)
                  :cbWndExtra (foreign-type-size :long))
  (register-class "Checker4Win" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "Checker4Win" "Checker4 Mouse Hit-Test Demo")))
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
