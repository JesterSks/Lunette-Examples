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

(in-package :Lunette.Examples.blokout2)

(defparameter fBlocking nil)
(defparameter fValidBox nil)
(defparameter ptBeg (cons nil nil))
(defparameter ptEnd (cons nil nil))
(defparameter ptBoxBeg (cons nil nil))
(defparameter ptBoxEnd (cons nil nil))

(defun DrawBoxOutline (hdc ptBeg ptEnd)
  (SetROP2 hdc R2_NOT)
  (SelectObject hdc (GetStockObject NULL_BRUSH))
  (Rectangle hdc (car ptBeg) (cdr ptBeg) (car ptEnd) (cdr ptEnd)))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_LBUTTONDOWN)
    (setf (car ptBeg) (setf (car ptEnd) (loword lparam))
          (cdr ptBeg) (setf (cdr ptEnd) (hiword lparam)))

    (with-dc hdc hWnd
             (DrawBoxOutline hdc ptBeg ptEnd))
    (SetCapture hWnd)
    (SetCursor (LoadCursor (null-pointer) IDC_CROSS))
    (setf fBlocking t)
    0)
   ((eql msg WM_MOUSEMOVE)
    (when fBlocking
      (SetCursor (LoadCursor (null-pointer) IDC_CROSS))
      (with-dc hdc hWnd
               (DrawBoxOutline hdc ptBeg ptEnd)
               (setf (car ptEnd) (loword lparam)
                     (cdr ptEnd) (hiword lparam))
               (DrawBoxOutline hdc ptBeg ptEnd)))
    0)
   ((eql msg WM_LBUTTONUP)
    (when fBlocking
      (with-dc hdc hWnd
               (DrawBoxOutline hdc ptBeg ptEnd))
      (setf (car ptBoxBeg) (car ptBeg)
            (cdr ptBoxBeg) (cdr ptBeg)
            (car ptBoxEnd) (loword lparam)
            (cdr ptBoxEnd) (hiword lparam)
            fBlocking nil
            fValidBox t)

      (ReleaseCapture)
      (SetCursor (LoadCursor (null-pointer) IDC_ARROW))

      (InvalidateRect hwnd (null-pointer) 1))
    0)
   ((eql msg WM_CHAR)
    (when (and fBlocking (eql (code-char wparam) #\ESC))
      (with-dc hdc hWnd
               (DrawBoxOutline hdc ptBeg ptEnd))
      (ReleaseCapture)
      (SetCursor (LoadCursor (null-pointer) IDC_ARROW))
      (setf fBlocking nil))
    0)
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (when fValidBox
               (SelectObject hdc (GetStockObject BLACK_BRUSH))
               (Rectangle hdc (car ptBoxBeg) (cdr ptBoxBeg)
                          (car ptBoxEnd) (cdr ptBoxEnd)))
             (when fBlocking
               (DrawBoxOutline hdc ptBeg ptEnd)))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "BlokOut2Win" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "BlokOut2Win" "Mouse Button Demo")))
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
