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

(in-package :Lunette.Examples.owndraw)

(defconstant ID_SMALLER 1)
(defconstant ID_LARGER  2)

(defconstant BTN_WIDTH  8)
(defconstant BTN_HEIGHT 4)

(defparameter hwndSmaller nil)
(defparameter hwndLarger  nil)
(defparameter cxClient    nil)
(defparameter cyClient    nil)
(defparameter cxChar      nil)
(defparameter cyChar      nil)

(defun triangle (hdc pts)
  (with-foreign-object (points 'POINT 3)
                       (let ((aPt (aref pts 0))
                             (aPoint (mem-aref points 'POINT 0)))
                         (setf (foreign-slot-value aPoint 'POINT 'X) (car aPt)
                               (foreign-slot-value aPoint 'POINT 'Y) (cdr aPt)))
                       (let ((aPt (aref pts 1))
                             (aPoint (mem-aref points 'POINT 1)))
                         (setf (foreign-slot-value aPoint 'POINT 'X) (car aPt)
                               (foreign-slot-value aPoint 'POINT 'Y) (cdr aPt)))
                       (let ((aPt (aref pts 2))
                             (aPoint (mem-aref points 'POINT 2)))
                         (setf (foreign-slot-value aPoint 'POINT 'X) (car aPt)
                               (foreign-slot-value aPoint 'POINT 'Y) (cdr aPt)))
                       (SelectObject hdc (GetStockObject BLACK_BRUSH))
                       (Polygon hdc points 3)
                       (SelectObject hdc (GetStockObject WHITE_BRUSH))))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (setf cxChar (loword (GetDialogBaseUnits))
          cyChar (hiword (GetDialogBaseUnits)))

    (setf hwndSmaller (create-window-ex "button" ""
                                        :dwStyle (logior WS_CHILD WS_VISIBLE BS_OWNERDRAW)
                                        :x 0 :y 0
                                        :nWidth (* BTN_WIDTH cxChar)
                                        :nHeight (* BTN_HEIGHT cyChar)
                                        :hWndParent hwnd
                                        :hMenu (make-pointer ID_SMALLER))
          hwndLarger (create-window-ex "button" ""
                                       :dwStyle (logior WS_CHILD WS_VISIBLE BS_OWNERDRAW)
                                       :x 0 :y 0
                                       :nWidth (* BTN_WIDTH cxChar)
                                       :nHeight (* BTN_HEIGHT cyChar)
                                       :hWndParent hwnd
                                       :hMenu (make-pointer ID_LARGER)))
    0)
   ((eql msg WM_SIZE)
    (setf cxClient (loword lparam)
          cyClient (hiword lparam))

    (MoveWindow hwndSmaller (- (floor (/ cxClient 2)) (* 3 (floor (/ (* BTN_WIDTH cxChar) 2))))
                (- (floor (/ cyClient 2)) (/ (* BTN_HEIGHT cyChar) 2))
                (* BTN_WIDTH cxChar)
                (* BTN_HEIGHT cyChar) 1)
    (MoveWindow hwndLarger (+ (floor (/ cxClient 2)) (* 3 (floor (/ (* BTN_WIDTH cxChar) 2))))
                (- (floor (/ cyClient 2)) (/ (* BTN_HEIGHT cyChar) 2))
                (* BTN_WIDTH cxChar)
                (* BTN_HEIGHT cyChar) 1)
    0)
   ((eql msg WM_COMMAND)
    (with-foreign-object (rc 'RECT)
                         (GetWindowREct hwnd rc)

                         (cond
                          ((eql wParam ID_SMALLER)
                           (incf (foreign-slot-value rc 'RECT 'left)   (floor (/ cxClient 20)))
                           (decf (foreign-slot-value rc 'RECT 'right)  (floor (/ cxClient 20)))
                           (incf (foreign-slot-value rc 'RECT 'top)    (floor (/ cyClient 20)))
                           (decf (foreign-slot-value rc 'RECT 'bottom) (floor (/ cyClient 20))))
                          ((eql wParam ID_LARGER)
                           (decf (foreign-slot-value rc 'RECT 'left)   (floor (/ cxClient 20)))
                           (incf (foreign-slot-value rc 'RECT 'right)  (floor (/ cxClient 20)))
                           (decf (foreign-slot-value rc 'RECT 'top)    (floor (/ cyClient 20)))
                           (incf (foreign-slot-value rc 'RECT 'bottom) (floor (/ cyClient 20)))))

                         (MoveWindow hwnd (foreign-slot-value rc 'RECT 'left)
                                     (foreign-slot-value rc 'RECT 'top)
                                     (- (foreign-slot-value rc 'RECT 'right)
                                        (foreign-slot-value rc 'RECT 'left))
                                     (- (foreign-slot-value rc 'RECT 'bottom)
                                        (foreign-slot-value rc 'RECT 'top)) 1))
    0)
   ((eql msg WM_DRAWITEM)
    (let* ((pdis (make-pointer lParam))
           (hdc (foreign-slot-value pdis 'DRAWITEMSTRUCT 'hDC))
           (ctlId (foreign-slot-value pdis 'DRAWITEMSTRUCT 'CtlID))
           (itemState (foreign-slot-value pdis 'DRAWITEMSTRUCT 'itemState))
           (rect (foreign-slot-value pdis 'DRAWITEMSTRUCT 'rcItem))
           (cx (- (foreign-slot-value rect 'RECT 'right)
                  (foreign-slot-value rect 'RECT 'left)))
           (cy (- (foreign-slot-value rect 'RECT 'bottom)
                  (foreign-slot-value rect 'RECT 'top))))
      (FillRect hdc rect (GetStockObject WHITE_BRUSH))
      (FrameRect hdc rect (GetStockObject BLACK_BRUSH))

      (cond
       ((eql ctlId ID_SMALLER)
        (Triangle hdc (vector (cons (* 3 (floor (/ cx 8))) (* 1 (floor (/ cy 8))))
                              (cons (* 5 (floor (/ cx 8))) (* 1 (floor (/ cy 8))))
                              (cons (* 4 (floor (/ cx 8))) (* 3 (floor (/ cy 8))))))

        (Triangle hdc (vector (cons (* 7 (floor (/ cx 8))) (* 3 (floor (/ cy 8))))
                              (cons (* 7 (floor (/ cx 8))) (* 5 (floor (/ cy 8))))
                              (cons (* 5 (floor (/ cx 8))) (* 4 (floor (/ cy 8))))))

        (Triangle hdc (vector (cons (* 5 (floor (/ cx 8))) (* 7 (floor (/ cy 8))))
                              (cons (* 3 (floor (/ cx 8))) (* 7 (floor (/ cy 8))))
                              (cons (* 4 (floor (/ cx 8))) (* 5 (floor (/ cy 8))))))

        (Triangle hdc (vector (cons (* 1 (floor (/ cx 8))) (* 5 (floor (/ cy 8))))
                              (cons (* 1 (floor (/ cx 8))) (* 3 (floor (/ cy 8))))
                              (cons (* 3 (floor (/ cx 8))) (* 4 (floor (/ cy 8)))))))
       ((eql ctlId ID_LARGER)
        (Triangle hdc (vector (cons (* 5 (floor (/ cx 8))) (* 3 (floor (/ cy 8))))
                              (cons (* 3 (floor (/ cx 8))) (* 3 (floor (/ cy 8))))
                              (cons (* 4 (floor (/ cx 8))) (* 1 (floor (/ cy 8))))))

        (Triangle hdc (vector (cons (* 5 (floor (/ cx 8))) (* 5 (floor (/ cy 8))))
                              (cons (* 5 (floor (/ cx 8))) (* 3 (floor (/ cy 8))))
                              (cons (* 7 (floor (/ cx 8))) (* 4 (floor (/ cy 8))))))

        (Triangle hdc (vector (cons (* 3 (floor (/ cx 8))) (* 5 (floor (/ cy 8))))
                              (cons (* 5 (floor (/ cx 8))) (* 5 (floor (/ cy 8))))
                              (cons (* 4 (floor (/ cx 8))) (* 7 (floor (/ cy 8))))))

        (Triangle hdc (vector (cons (* 3 (floor (/ cx 8))) (* 3 (floor (/ cy 8))))
                              (cons (* 3 (floor (/ cx 8))) (* 5 (floor (/ cy 8))))
                              (cons (* 1 (floor (/ cx 8))) (* 4 (floor (/ cy 8))))))))

      (when (not (= 0 (logand itemState ODS_SELECTED)))
        (InvertRect hdc rect))

      (when (not (= 0 (logand itemState ODS_FOCUS)))
        (incf (foreign-slot-value rect 'RECT 'left) (floor (/ cx 16)))
        (incf (foreign-slot-value rect 'RECT 'top) (floor (/ cx 16)))
        (decf (foreign-slot-value rect 'RECT 'right) (floor (/ cy 16)))
        (decf (foreign-slot-value rect 'RECT 'bottom) (floor (/ cy 16)))

        (DrawFocusRect hdc rect)))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "OwnDrawWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "OwnDrawWin" "Owner-Draw Button Demo")))
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
