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

(in-package :Lunette.Examples.Char)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
    ((eql msg WM_DESTROY)
     (PostQuitMessage 0)
     0)
    ((eql msg WM_CHAR)
     (let ((msg (format nil "Character is ~A~%" (code-char wparam))))
       (message-box hwnd "WM_CHAR Received" msg)))
    (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "WmCharWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "WmCharWin" "WM_CHAR Window")))
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
