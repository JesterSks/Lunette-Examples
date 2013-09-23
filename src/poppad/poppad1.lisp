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

(in-package :Lunette.Examples.poppad1)

(defconstant ID_EDIT 1)
(defparameter hwndEdit nil)

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (with-foreign-object (rect 'RECT)
                         (GetClientRect hWnd rect)
                         (let ((cxClient (- (foreign-slot-value rect 'RECT 'right)
                                            (foreign-slot-value rect 'RECT 'left)))
                               (cyClient (- (foreign-slot-value rect 'RECT 'bottom)
                                            (foreign-slot-value rect 'RECT 'top))))

                           (setf hwndEdit (create-window-ex "edit" ""
                                                            :dwStyle (logior WS_CHILD WS_VISIBLE
                                                                             WS_HSCROLL WS_VSCROLL
                                                                             WS_BORDER ES_LEFT ES_MULTILINE
                                                                             ES_AUTOHSCROLL ES_AUTOVSCROLL)
                                                            :x 0 :y 0
                                                            :nWidth cxClient
                                                            :nHeight cyClient
                                                            :hWndParent hwnd
                                                            :hMenu (make-pointer ID_EDIT)))))
    0)
   ((eql msg WM_SIZE)
    (let ((cxClient (loword lparam))
          (cyClient (hiword lparam)))
      (MoveWindow hwndEdit 0 0 cxClient cyClient 1))
    0)
   ((eql msg WM_SETFOCUS)
    (SetFocus hwndEdit)
    0)
   ((eql msg WM_COMMAND)
    (when (= ID_EDIT (loword wParam))
      (when (or (= EN_ERRSPACE (hiword wParam))
                (= EN_MAXTEXT (hiword wParam)))
        (message-box hwnd "Edit control out of space." "PopPad1" (or MB_OK MB_ICONSTOP))))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "PopPad1Win" :lpfnWndProc (callback WindowFunc))

  (setf rcColor (foreign-alloc 'RECT))

  (unwind-protect
      (let ((hwnd (create-window-ex "PopPad1Win" "PopPad1")))
        (ShowWindow hwnd SW_SHOW)
        (ShowWindow hwnd SW_SHOW)
        (UpdateWindow hwnd)

        (with-foreign-object (msg 'MSG)
                             (loop with result
                                   while (not (= 0 (setf result (GetMessage MSG (null-pointer) 0 0))))
                                   do (progn
                                        (TranslateMessage msg)
                                        (DispatchMessage msg)))
                             (foreign-slot-value msg 'MSG 'wParam)))
    (foreign-free rcColor)))
