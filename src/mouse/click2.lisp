#|
   Copyright 2014 Robert Burghart

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

(in-package :Lunette.Examples.Click2)

(defvar *original-double-click-time* 0)

(defun update-double-click-time (hwnd interval)
  (when (< interval 0)
    (setf interval 0))
  (SetDoubleClickTime interval)
  (let ((str (format nil "New interval is ~A milliseconds" interval)))
    (message-box hWnd "Setting Double-Click Interval" str)))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                     (msg :UINT)
                                     (wparam WPARAM)
                                     (lparam LPARAM))
  (cond
    ((eql msg WM_DESTROY)
     (SetDoubleClickTime *original-double-click-time*)
     (PostQuitMessage 0)
     0)
    ((eql msg WM_KEYDOWN)
     (cond ((eql wparam VK_UP)
            (update-double-click-time hwnd (+ (GetDoubleClickTime) 100)))
           ((eql wparam VK_DOWN)
            (update-double-click-time hwnd (- (GetDoubleClickTime) 100))))
     0)
    ((eql msg WM_RBUTTONDOWN)
     (let* ((hdc (GetDC hWnd))
            (locx (loword lparam))
            (locy (hiword lparam))
            (str (format nil "Right button is down at ~A, ~A" locx locy)))
       (with-foreign-string (cstr str)
           (TextOut hdc locx locy cstr (length str)))
       (ReleaseDC hWnd hdc))
     0)
    ((eql msg WM_LBUTTONDOWN)
     (let* ((hdc (GetDC hWnd))
            (locx (loword lparam))
            (locy (hiword lparam))
            (str (format nil "Left button is down at ~A, ~A" locx locy)))
       (with-foreign-string (cstr str)
           (TextOut hdc locx locy cstr (length str)))
       (ReleaseDC hWnd hdc))
     0)
    ((eql msg WM_LBUTTONDBLCLK)
     (let ((str (format nil "Left Button~%Interval is ~A milliseconds"
                        (GetDoubleClickTime))))
       (message-box hwnd "Double Click" str))
     0)
    ((eql msg WM_RBUTTONDBLCLK)
     (let ((str (format nil "Right Button~%Interval is ~A milliseconds"
                        (GetDoubleClickTime))))
       (message-box hWnd "Double Click" str))
     0)
    (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16)

  (register-class "Click2Win" :lpfnWndProc (callback WindowFunc)
                  :style (logior CS_HREDRAW CS_VREDRAW CS_DBLCLKS))

  (let ((hwnd (create-window-ex "Click2Win" "Click 2 Window")))
    (setf *original-double-click-time* (GetDoubleClickTime))

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
