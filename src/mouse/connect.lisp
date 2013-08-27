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

(in-package :Lunette.Examples.connect)

(defconstant MAXPOINTS 1000)

(defparameter pt (make-array MAXPOINTS :element-type 'cons))
(defparameter iCount 0)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_LBUTTONDOWN)
    (setf iCount 0)
    (InvalidateRect hWnd (null-pointer) 1)
    0)
   ((eql msg WM_MOUSEMOVE)
    (when (and (not (= 0 (logand wParam MK_LBUTTON)))
               (< iCount MAXPOINTS))
      (setf (car (aref pt iCount)) (loword lParam)
            (cdr (aref pt iCount)) (hiword lParam))

      (incf iCount)

      (with-dc hdc hWnd
               (SetPixel hdc (loword lParam) (hiword lParam) 0)))
    0)
   ((eql msg WM_LBUTTONUP)
    (InvalidateRect hWnd (null-pointer) 0)
    0)
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (SetCursor (LoadCursor (GetModuleHandle (null-pointer)) IDC_WAIT))
             (ShowCursor 1)

             (loop for i
                   from 0
                   to (1- iCount)
                   do (let ((startPt (aref pt i)))
                        (loop for j
                              from 0
                              to (1- iCount)
                              do (let ((endPt (aref pt j)))
                                   (MoveToEx hdc (car startPt)
                                             (cdr startPt) (null-pointer))
                                   (LineTo hdc (car endPt)
                                           (cdr endPt))))))

             (ShowCursor 0)
             (SetCursor (LoadCursor (GetModuleHandle (null-pointer)) IDC_ARROW)))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (loop for i
        from 0
        to (1- MAXPOINTS)
        do (setf (aref pt i) (cons 0 0)))

  (register-class "ConnectWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "ConnectWin" "Connect-the-Points Mouse Demo")))
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
