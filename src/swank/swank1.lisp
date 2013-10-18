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

(in-package :Lunette.Examples.swank)

(defparameter running t)

(defun window-func (hwnd msg wparam lparam)
  (declare (ignore wparam lparam))

  (cond
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (text-out hdc 0 0 "Hello Test!")
             (text-out hdc 0 20 "Hello Dynamic Change!"))
    0)))

(defcallback GenWinFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (let ((result (funcall #'window-func hwnd msg wparam lparam)))
    (or result
        (DefWindowProc hwnd msg wparam lparam))))

(defun handle-swank ()
  (let ((connection (or swank::*emacs-connection*
                        (swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

(defun my-main ()
  (setf *default-foreign-encoding* :utf-16le)

  (with-foreign-object (msg 'MSG)
                       (loop while running
                             do (with-simple-restart (continue "Continue Win32 loop.")
                                                     (unless (= 0 (PeekMessage MSG (null-pointer) 0 0 PM_REMOVE))
                                                       (TranslateMessage msg)
                                                       (DispatchMessage msg))
                                                     (handle-swank)))
                       (foreign-slot-value msg 'MSG 'wParam)))
