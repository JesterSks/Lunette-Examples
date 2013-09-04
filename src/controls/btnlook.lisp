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

(in-package :Lunette.Examples.btnlook)

(defparameter button (vector (cons BS_PUSHBUTTON      "PUSHBUTTON")
                             (cons BS_DEFPUSHBUTTON   "DEFPUSHBUTTON")
                             (cons BS_CHECKBOX        "CHECKBOX")
                             (cons BS_AUTOCHECKBOX    "AUTOCHECKBOX")
                             (cons BS_RADIOBUTTON     "RADIOBUTTON")
                             (cons BS_3STATE          "3STATE")
                             (cons BS_AUTO3STATE      "AUTO3STATE")
                             (cons BS_GROUPBOX        "GROUPBOX")
                             (cons BS_AUTORADIOBUTTON "AUTORADIO")
                             (cons BS_OWNERDRAW       "OWNERDRAW")))

(defconstant title (format nil "~16@<~A~>~13@<~A~>~9@<~A~>" "message" "wParam" "lParam"))
(defconstant titleLines (format nil "~16@<~A~>~13@<~A~>~9@<~A~>" "_______" "______" "______"))
(defconstant lineFormat "~16@<~A~>~13@<~4,'0X-~4,'0X~>~9@<~4,'0X-~4,'0X~>")

(defparameter hwndButton (make-array 10 :initial-element nil))
(defparameter listRect nil)

(defparameter cxChar 0)
(defparameter cyChar 0)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (setf cxChar (loword (GetDialogBaseUnits))
          cyChar (hiword (GetDialogBaseUnits)))

    (GetClientRect hWnd listRect)
    (setf (foreign-slot-value listRect 'RECT 'left) (* 24 cxChar)
          (foreign-slot-value listRect 'RECT 'top) (* 2 cyChar))

    (loop for i from 0 to 9
          do (setf (aref hwndButton i)
                   (create-window-ex "button" (cdr (aref button i))
                                     :dwStyle (logior WS_CHILD WS_VISIBLE (car (aref button i)))
                                     :x cxChar
                                     :y (* cyChar (1+ (* 2 i)))
                                     :nWidth (* cxChar 20)
                                     :nHeight (* 7 (floor (/ cyChar 4)))
                                     :hWndParent hwnd
                                     :hMenu (make-pointer i))))
    0)
   ((eql msg WM_SIZE)
    (setf (foreign-slot-value listRect 'RECT 'right) (loword lparam)
          (foreign-slot-value listRect 'RECT 'bottom) (hiword lparam))
    0)
   ((eql msg WM_PAINT)
    (InvalidateRect hwnd listRect 1)

    (with-ps ps hdc hWnd
             (SelectObject hdc (GetStockObject SYSTEM_FIXED_FONT))
             (SetBkMode hdc TRANSPARENT)

             (text-out hdc (* 24 cxChar) cyChar title)
             (text-out hdc (* 24 cxChar) cyChar titleLines))
    0)
   ((or (eql msg WM_DRAWITEM)
        (eql msg WM_COMMAND))
    (ScrollWindow hwnd 0 (- cyChar) listRect listRect)

    (with-dc hdc hWnd
             (SelectObject hdc (GetStockObject SYSTEM_FIXED_FONT))

             (text-out hdc (* 24 cxChar)
                       (* cyChar (1- (floor (/ (foreign-slot-value listRect 'RECT 'bottom) cyChar))))
                       (format nil lineFormat (if (eql msg WM_DRAWITEM)
                                                  "WM_DRAWITEM"
                                                "WM_COMMAND")
                               (hiword wParam) (loword wParam)
                               (hiword lParam) (loword lParam))))
    (ValidateRect hwnd listRect)
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (setf listRect (foreign-alloc 'RECT))

  (register-class "BtnLookWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "BtnLookWin" "Button Look")))
    (ShowWindow hwnd SW_SHOW)
    (ShowWindow hwnd SW_SHOW)
    (UpdateWindow hwnd)

    (with-foreign-object (msg 'MSG)
                         (loop with result
                               while (not (= 0 (setf result (GetMessage MSG (null-pointer) 0 0))))
                               do (progn
                                    (TranslateMessage msg)
                                    (DispatchMessage msg)))
                         (foreign-free listRect)
                         (foreign-slot-value msg 'MSG 'wParam))))
