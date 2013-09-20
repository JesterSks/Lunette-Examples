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

(in-package :Lunette.Examples.colors1)

(defparameter crPrime (vector (rgb 255   0   0)
                              (rgb   0 255   0)
                              (rgb   0   0 255)))
(defparameter hBrush (vector 0 0 0))
(defparameter hBrushStatic)
(defparameter hwndScroll (vector nil nil nil))
(defparameter hwndLabel (vector nil nil nil))
(defparameter hwndValue (vector nil nil nil))
(defparameter hwdRect nil)
(defparameter color (vector 0 0 0))
(defparameter cyChar)
(defparameter rcColor)
(defparameter szColorLabel (vector "Red" "Green" "Blue"))
(defparameter idFocus nil)
(defparameter OldScroll (vector nil nil nil))

(defcallback ScrollProc LRESULT ((hWnd HWND)
                                 (msg :UNIT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (let ((id (GetWindowLong hwnd GWL_ID)))
    (cond
     ((eql msg WM_KEYDOWN)
      (when (= wparam VK_TAB)
        (SetFocus (GetDlgItem (GetParent hwnd)
                              (+ id (mod (if (< 0 (GetKeyState VK_SHIFT)) 2 1) 3))))))
     ((eql msg WM_SETFOCUS)
      (setf idFocus id)))

    (CallWindowProc (aref OldScroll id) hwnd msg wparam lparam)))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (let ((hInstance (GetWindowLong hwnd GWL_HINSTANCE)))
      (setf hwndRect (create-window-ex "static" ""
                                       :dwStyle (logior WS_CHILD WS_VISIBLE SS_WHITERECT)
                                       :x 0 :y 0 :nWidth 0 :nHeight 0
                                       :hWndParent hwnd
                                       :hMenu (make-pointer 9)))

      (loop for i from 0 below 3
            do (progn
                 (setf (aref hwndScroll i) (create-window-ex "scrollbar" ""
                                                             :dwStyle (logior WS_CHILD WS_VISIBLE
                                                                              WS_TABSTOP SBS_VERT)
                                                             :x 0 :y 0 :nWidth 0 :nHeight 0
                                                             :hWndParent hwnd
                                                             :hMenu (make-pointer i)))
                 (SetScrollRange (aref hwndScroll i) SB_CTL 0 255 0)
                 (SetScrollPos (aref hwndScroll i) SB_CTL 0 0)

                 (setf (aref hwndLabel i) (create-window-ex "static"
                                                            (aref szColorLabel i)
                                                            :dwStyle (logior WS_CHILD WS_VISIBLE SS_CENTER)
                                                            :x 0 :y 0 :nWidth 0 :nHeight 0
                                                            :hWndParent hwnd
                                                            :hMenu (make-pointer (+ i 3))))

                 (setf (aref hwndValue i) (create-window-ex "static" "0"
                                                            :dwStyle (logior WS_CHILD WS_VISIBLE SS_CENTER)
                                                            :x 0 :y 0 :nWidth 0 :nHeight 0
                                                            :hWndParent hwnd
                                                            :hMenu (make-pointer (+ i 6))))

                 (setf (aref OldScroll i) (SetWindowLong (aref hwndScroll i)
                                                         GWL_WNDPROC (callback ScrollProc)))
                 (setf (aref hBrush i) (CreateSolidBrush (aref crPrime i)))))

      (setf hBrushStatic (CreateSolidBrush (GetSysColor COLOR_BTNHIGHLIGHT)))
      (setf cyChar (hiword (GetDialogBaseUnits))))
    0)
   ((eql msg WM_SIZE)
    (let ((cxClient (loword lparam))
          (cyClient (hiword lparam)))
      (SetRect rcColor (floor (/ cxClient 2)) 0 cxClient cyClient)

      (MoveWindow hwndRect 0 0 (floor (/ cxClient 2)) cyClient 1)

      (loop for i from 0 below 3
            do (progn
                 (MoveWindow (aref hwndScroll i)
                             (* (1+ (* 2 i)) (floor (/ cxClient 14)))
                             (* 2 cyChar)
                             (floor (/ cxClient 14))
                             (- cyClient (* 4 cyChar))
                             1)

                 (MoveWindow (aref hwndScroll i)
                             (* (1+ (* 4 i)) (floor (/ cxClient 28)))
                             (floor (/ cyChar 2))
                             (floor (/ cxClient 7))
                             cyChar
                             1)

                 (MoveWindow (aref hwndValue i)
                             (* (1+ (* 4 i)) (floor (/ cxClient 28)))
                             (- cyClient (* 3 (floor (/ cyChar 2))))
                             (floor (/ cxClient 7))
                             cyChar
                             1))))
    (SetFocus hwnd)
    0)
   ((eql msg WM_SETFOCUS)
    (SetFocus (aref hwndScroll idFocus))
    0)
   ((eql msg WM_VSCROLL)
    (let ((i (GetWindowLong (make-pointer lParam) GWL_ID))
          (msg (loword wparam)))
      (cond
       ((eql msg SB_PAGEDOWN)
        (incf (aref color i) 15))
       ((eql msg SB_LINEDOWN)
        (setf (aref color i) (min 255 (1+ (aref color i)))))
       ((eql msg SB_PAGEUP)
        (decf (aref color i) 15))
       ((eql msg SB_LINEUP)
        (setf (aref color i) (max 0 (1- (aref color i)))))
       ((eql msg SB_TOP)
        (setf (aref color i) 0))
       ((eql msg SB_BUTTOM)
        (setf (aref color i) 255))
       ((or (eql msg SB_THUMBPOSITION)
            (eql msg SB_THUMBTRACK))
        (setf (aref color i) (hiword wparam))))

      (SetScrollPos (aref hwndScroll i) SB_CTL (aref color i) 1)
      (SetWindowText (aref hwndValue i) (format nil "~A" (aref color i))))

    (DeleteObject (make-pointer (SetClassLong hwnd CGL_HBRBACKGROUND
                                              (CreateSolidBrush (rgb (aref color 0)
                                                                     (aref color 1)
                                                                     (aref color 2))))))
    (InvalidateRect hwnd rcColor 1)
    0)
   ((eql msg WM_CTLCOLORSCROLLBAR)
    (let ((i (GetWindowLong (make-pointer lparam) GWL_ID)))
      (aref hBrush i)))
   ((eql msg WM_CTLCOLORSTATIC)
    (let ((i (GetWindowLong (make-pointer lparam) GWL_ID))
          (hdc (make-pointer wparam)))
      (loop for i from 3 to 8
            do)
      (if (and (>= i 3)
               (<= i 8))
          (progn
            (SetTextColor hdc (aref crPrime (mod i 3)))
            (SetBkColor hdc (GetSysColor COLOR_BTNHIGHLIGHT))
            hBrushStatic)
        (DefWindowProc hwnd msg wparam lparam))))
   ((eql msg WM_SYSCOLORCHANGE)
    (DeleteObject hBrushStatic)
    (setf hBrushStatic (CreateSolidBrush (GetSysColor COLOR_BTNHIGHLIGHT)))
    0)
   ((eql msg WM_DESTROY)
    (DeleteObject (make-pointer (SetClassLong hwnd GCL_HBRBACKGROUND
                                              (GetStockObject WHITE_BRUSH))))

    (loop for i from 0 below 3
          do (DeleteObject (aref hBrush i)))

    (DeleteObject hBrushStatic)

    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "Colors1Win" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "Colors1Win" "Color Scroll")))
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
