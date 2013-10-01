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

(defcallback ListProc LRESULT ((hWnd HWND)
                               (msg :UINT)
                               (wparam WPARAM)
                               (lparam LPARAM))
  (when (and (eql msg WM_KEYDOWN)
             (= wParam VK_RETURN))
    (SendMessage (GetParent hwnd) WM_COMMAND (makelong 1 LBN_DBLCLK) (pointer-address hwnd)))

  (CallWindowProc OldList hwnd msg wParam lParam))

(defun get-current-selection (hwndList)
  (let ((i (SendMessage hwndList LB_GETCURSEL 0 0)))
    (unless (= i LB_ERR)
      (let ((iLength (SendMessage hwndList LB_GETTEXTLEN i 0)))
        (with-foreign-pointer-as-string (pVarName (* 2 (1+ iLength)))
                                        (SendMessage hwndList LB_GETTEXT
                                                     iIndex (pointer-address pVarName)))))))

(defun valid-file? (filename)
  (with-foreign-string (cfilename filename)
                       (let ((hFile (CreateFile cfilename GENERIC_READ FILE_SHARED_READ
                                                (null-pointer) OPEN_EXISTING 0
                                                (null-pointer))))
                         (unless (= INVALID_HANDLE_VALUE hFile)
                           (CloseHandle hFile)
                           filename))))

(defun make-full-path (filename)
  (let ((curDir (with-foreign-pointer-as-string (cbuffer (* 2 (1+ MAX_PATH)))
                                                (GetCurrentDirectory (1+ MAX_PATH) cbuffer))))
    (if (eql #\\ (aref curDir (1- (length curDir))))
        (format nil "~A~A" curDir filename)
      (format nil "~A\\~A" curDir filename))))

(defun win32-read-file (filename)
  (with-foreign-string (cFile filename)
                       (let ((hFile (CreateFile cFile GENERIC_READ FILE_SHARED_READ
                                                (null-pointer) OPEN_EXISTING 0
                                                (null-pointer))))
                         (unless (= INVALID_HANDLE_VALUE hFile)
                           (with-foreign-pointer-as-string (cbuffer MAXREAD :encoding :ascii)
                                                           (with-foreign-object (i 'DWORD)
                                                                                (ReadFile hFile cbuffer MAXREAD i (null-pointer))
                                                                                (CloseHandle hFile)))))))

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

    (setf OldList (make-pointer (SetWindowLong hwndList GWL_WNDPROD (pointer-address (callback ListProc)))))
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
    (when (and (= (loword wParam) ID_LIST)
               (= (hiword wParam) LBN_DBLCLK))
      (let ((selStr (get-current-selection hwndList)))
        (when selStr
          (if (valid-file? selStr)
              (with-foreign-string (cTitle (make-full-path selStr))
                                   (setf bValidFile t
                                         szFile selStr)
                                   (SetWindowText hwndText cTitle))
            (progn
              (setf bValidFile nil)
              (with-foreign-string (cName selStr)
                                   (unless (= 0 (SetCurrentDirectory cName))
                                     (with-foreign-string (cDrive (format nil "~A:" selStr))
                                                          (SetCurrentDirectory cDrive))))
              (with-foreign-pointer-as-string (szBuffer (* 2 (1+ MAX_PATH)))
                                              (GetCurrentDirectory (1+ MAX_PATH) szBuffer)
                                              (SetWindowText hwndText szBuffer))
              (SendMessage hwndList LB_RESETCONTENT 0 0)
              (with-foreign-string (cStr "*.*")
                                   (SendMessage hwndList LB_DIR DIRATTR cStr))))
          (InvalidateRect hwnd (null-pointer) 1))))
    (InvalidateRect hwnd (null-pointer) 1)
    0)
   ((eql msg WM_PAINT)
    (when bValidFile
      (let ((contents (win32-read-file szFile)))
        (when contents
          (with-ps ps hdc hWnd
                   (SelectObject hdc (GetStockObject SYSTEM_FIXED_FONT))
                   (SetTextColor hdc (GetSysColor COLOR_BTNTEXT))
                   (SetBkColor   hdc (GetSysColor COLOR_BTNFACE))

                   (draw-text hdc contents (length contents) rect DTFLAGS)))))
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
