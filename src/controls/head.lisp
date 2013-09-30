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

(in-package :Lunette.Examples.head)

(defconstant ID_LIST 1)
(defconstant ID_TEXT 2)

(defconstant MAXREAD 8192)

(defconstant DIRATTR (logior DDL_READWRITE DDL_READONLY DDL_HIDDEN DDL_SYSTEM
                             DDL_DIRECTORY DDL_ARCHIVE  DDL_DRIVES))
(defconstant DTFLAGS (logior DT_WORDBREAK DT_EXPANDTABS DT_NOCLIP DT_NOPREFIX))

(defparameter OldList nil)

(defparameter rect nil)
(defparameter hwndList nil)
(defparameter hwndText nil)
(defparameter bValidFile nil)
(defparameter szFile nil)
(defparameter buffer nil)

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

(defcallback ListProc LRESULT ((hWnd HWND)
                               (msg :UINT)
                               (wparam WPARAM)
                               (lparam LPARAM))
  )

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_CREATE)
    (let ((cyChar (hiword (GetDialogBaseUnits)))
          (cxChar (loword (GetDialogBaseUnits))))

      (setf (foreign-slot-value rect 'RECT 'left) (* 20 cxChar)
            (foreign-slot-value rect 'RECT 'top)  (* 3  cyChar)

            hwndList (create-window-ex "listbox" ""
                                       :dwStyle (logior WS_CHILD WS_VISIBLE LBS_STANDARD)
                                       :x cxChar
                                       :y (* 3 cyChar)
                                       :nWidth (+ (* cxChar 13) (GetSystemMetrics SM_CXVSCROLL))
                                       :nHeight (* cyChar 10)
                                       :hWndParent hwnd
                                       :hMenu (make-pointer ID_LIST)))

      (let ((szBuffer (with-foreign-pointer-as-string (cbuffer (* 2 (1+ MAX_PATH)))
                                                      (GetCurrentDirectory (1+ MAX_PATH) cbuffer))))
        (setf hwndText (create-window-ex "static" szBuffer
                                         :dwStyle (logior WS_CHILD WS_VISIBLE SS_LEFT)
                                         :x cxChar
                                         :y cyChar
                                         :nWidth (* cxChar MAX_PATH)
                                         :nHeight cyChar
                                         :hWndParent hwnd
                                         :hMenu (make-pointer ID_TEXT)))))

    (setf OldList (SetWindowLong hwndList GWL_WNDPROD (pointer-address (callback ListProc))))
    (with-foreign-string (cstr "*.*")
                         (SendMessage hwndList LB_DIR DIRATTR (pointer-address cstr)))
    0)
   ((eql msg WM_SIZE)
    (setf (foreign-slot-value rect 'RECT 'right)  (loword lparam)
          (foreign-slot-value rect 'RECT 'bottom) (hiword lparam))
    0)
   ((eql msg WM_SETFOCUS)
    (SetFocus hwndList)
    0)
   ((eql msg WM_COMMAND)
    (block nil
      (when (and (= (loword wParam) ID_LIST)
                 (= (hiword wParam) LBN_DBLCLK))
        (let ((i (SendMessage hwndList LB_GETCURSEL 0 0)))
          (when (= i LB_ERR)
            (return))

          (with-foreign-pointer-as-string (cbuffer (* 2 (1+ MAX_PATH)))
                                          (SendMessage hwndList LB_GETTEXT i (pointer-address cbuffer))
                                          (let ((hFile (CreateFile cbuffer GENERIC_READ FILE_SHARED_READ
                                                                   (null-pointer) OPEN_EXISTING 0
                                                                   (null-pointer))))
                                            (if (not (= INVALID_HANDLE_VALUE hFile))
                                                (progn
                                                  (CloseHandle hFile)
                                                  (setf bValidFile t
                                                        szFile (foreign-string-to-lisp cbuffer))
                                                  (GetCurrentDirectory (1+ max_path) cbuffer)
                                                  (let ((strDir (foreign-string-to-lisp cbuffer)))
                                                    (unless (eql #\\ (aref strDir (1- (length strDir))))
                                                      (strDir (format nil "~A\\" strDir)))
                                                    (with-foreign-string (cTitle (concatenate 'string strDir szFile))
                                                                         (SetWindowText hwndText cTitle))))
                                              (progn
                                                (setf bValidFile nil)
                                                )))
           ))
        (let* ((pVarName (get-current-selection hwndList))
               (pVarValue (get-environment-variable pVarName)))
          (with-foreign-string (cstr pVarValue)
                               (SetWindowText hwndText cstr)))))
    (InvalidateRect hwnd (null-pointer) 1)
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "HeadWin" :lpfnWndProc (callback WindowFunc))

  (setf rect (foreign-alloc 'RECT))

  (unwind-protect
      (let ((hwnd (create-window-ex "HeadWin" "Head")))
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
    (foreign-free rect)))
