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

(in-package :Lunette.Examples.icon1)

(defparameter cxIcon 0)
(defparameter cyIcon 0)

(defparameter cxClient 0)
(defparameter cyClient 0)

(defconstant IDI_ICON 101)

(defparameter icon nil)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
    ((eql msg WM_CREATE)
     (setf cxIcon (GetSystemMetrics SM_CXICON)
           cyIcon (GetSystemMetrics SM_CYICON))
     0)
    ((eql msg WM_SIZE)
     (setf cxClient (loword lparam)
           cyClient (hiword lparam))
     0)
    ((eql msg WM_PAINT)
     (with-ps ps hdc hWnd
       (text-out hdc 0 0 "Hello World!"))
     0)
    ((eql msg WM_DESTROY)
     (PostQuitMessage 0)
     0)
    (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (with-library (rsrc "icon1.dll" "src/resources")

    (setf icon (LoadIcon rsrc (make-int-resource IDI_ICON)))

    (register-class "HelloPaintWin"
                    :lpfnWndProc (callback WindowFunc)
                    :hIcon icon)

    (let ((hwnd (create-window-ex "IconDemoWin" "Icon Window")))
      (ShowWindow hwnd SW_SHOW)
      (ShowWindow hwnd SW_SHOW)
      (UpdateWindow hwnd)

      (with-foreign-object (msg 'MSG)
        (loop with result
           while (not (= 0 (setf result (GetMessage MSG (null-pointer) 0 0))))
           do (progn
                (TranslateMessage msg)
                (DispatchMessage msg)))
        (foreign-slot-value msg 'MSG 'wParam)))))
