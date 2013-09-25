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

(in-package :Lunette.Examples.environ)

(defconstant ID_LIST 1)
(defconstant ID_TEXT 2)

(defparameter hwndList nil)
(defparameter hwndText nil)

(defun FillListBox (hwndList)
  (let ((pVarBlock (GetEnvironmentStrings)))
    (unwind-protect
        (loop with ptr = pVarBlock
              until (eql (mem-ref ptr :ushort) #\Null)
              do (multiple-value-bind (str byte-count) (foreign-string-to-lisp ptr)
                   (unless (eql (aref str 0) #\=)
                     (let ((splitPos (position #\= str)))
                       (when splitPos
                         (let ((varName (subseq str 0 splitPos)))

                           )))
                     )
                   (incf-pointer ptr (+ 2 byte-count))))
      (FreeEnvironmentStrings pVarBlock))))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (let ((cyChar (hiword (GetDialogBaseUnits)))
          (cxChar (loword (GetDialogBaseUnits)))))
    0)
   ((eql msg WM_SETFOCUS)
    (SetFocus hwndList)
    0)
   ((eql msg WM_COMMAND)
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "EnvironWin" :lpfnWndProc (callback WindowFunc))

  (setf rcColor (foreign-alloc 'RECT))

  (unwind-protect
      (let ((hwnd (create-window-ex "EnvironWin" "Environment List Box")))
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
