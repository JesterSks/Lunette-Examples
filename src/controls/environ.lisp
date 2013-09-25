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
              until (eql #\Null (code-char (mem-ref ptr :ushort)))
              do (multiple-value-bind (str byte-count) (foreign-string-to-lisp ptr)
                   (unless (or (= 0 byte-count)
                               (eql (aref str 0) #\=))
                     (let ((splitPos (position #\= str)))
                       (when splitPos
                         (let ((varName (subseq str 0 splitPos)))
                           (with-foreign-string (cstr varName)
                                                (SendMessage hwndList LB_ADDSTRING 0 (pointer-address cstr)))))))
                   (incf-pointer ptr (+ 2 byte-count))))
      (FreeEnvironmentStrings pVarBlock))))

(defun get-current-selection (hwndList)
  (let* ((iIndex (SendMessage hwndList LB_GETCURSEL 0 0))
         (iLength (SendMessage hwndList LB_GETTEXTLEN iIndex 0)))
    (with-foreign-pointer-as-string (pVarName (* 2 (1+ iLength)))
                                    (SendMessage hwndList LB_GETTEXT
                                                 iIndex (pointer-address pVarName)))))

(defun get-environment-variable (pVarName)
  (with-foreign-string (cstr pVarName)
                       (let ((iLength (GetEnvironmentVariable cstr (null-pointer) 0)))
                         (with-foreign-pointer-as-string (pVarValue (* 2 iLength))
                                                         (GetEnvironmentVariable cstr pVarValue iLength)))))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (let ((cyChar (hiword (GetDialogBaseUnits)))
          (cxChar (loword (GetDialogBaseUnits))))
      (setf hwndList (create-window-ex "listbox" ""
                                       :dwStyle (logior WS_CHILD WS_VISIBLE LBS_STANDARD)
                                       :x cxChar
                                       :y (* 3 cyChar)
                                       :nWidth (+ (* cxChar 16) (GetSystemMetrics SM_CXVSCROLL))
                                       :nHeight (* cyChar 5)
                                       :hWndParent hwnd
                                       :hMenu (make-pointer ID_LIST))
            hwndText (create-window-ex "static" ""
                                       :dwStyle (logior WS_CHILD WS_VISIBLE SS_LEFT)
                                       :x cxChar
                                       :y cyChar
                                       :nWidth (GetSystemMetrics SM_CXSCREEN)
                                       :nHeight cyChar
                                       :hWndParent hwnd
                                       :hMenu (make-pointer ID_TEXT)))

      (FillListBox hwndList))
    0)
   ((eql msg WM_SETFOCUS)
    (SetFocus hwndList)
    0)
   ((eql msg WM_COMMAND)
    (when (and (= (loword wParam) ID_LIST)
               (= (hiword wParam) LBN_SELCHANGE))
      (let* ((pVarName (get-current-selection hwndList))
             (pVarValue (get-environment-variable pVarName)))
        (with-foreign-string (cstr pVarValue)
                             (SetWindowText hwndText cstr))))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "EnvironWin" :lpfnWndProc (callback WindowFunc))

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
                         (foreign-slot-value msg 'MSG 'wParam))))
