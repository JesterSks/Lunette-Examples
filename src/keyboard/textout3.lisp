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

(in-package :Lunette.Examples.Textout3)

(defvar *last-str* "")

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                     (msg :UINT)
                                     (wparam WPARAM)
                                     (lparam LPARAM))
  (cond
    ((eql msg WM_DESTROY)
     (PostQuitMessage 0)
     0)
    ((eql msg WM_PAINT)
     (with-foreign-object (paint-struct 'PAINTSTRUCT)
       (let ((hdc (BeginPaint hWnd paint-struct)))
         (with-foreign-string (cstr *last-str*)
             (TextOut hdc 0 0 cstr (length *last-str*)))
         (EndPaint hWnd paint-struct)))
     0)
    ((eql msg WM_CHAR)
     (setf *last-str* (format nil "~A~A" *last-str* (code-char wparam)))
     (InvalidateRect hWnd (null-pointer) 1)
     0)
    (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "Textout3Win" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "Textout3Win" "Textout 3 Window")))
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
