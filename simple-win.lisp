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

(ql:quickload "cffi")

(defpackage :simple-win
  (:use :common-lisp
        :cffi))

(in-package :simple-win)

(define-foreign-library user32
  (:windows "user32.dll"))

(use-foreign-library user32)

(defconstant IDI_APPLICATION (make-pointer 32512))
(defconstant IDC_ARROW (make-pointer 32512))

(defconstant WHITE_BRUSH 0)

(defconstant CS_VREDRAW #x0001)
(defconstant CS_HREDRAW #x0002)

(defconstant WS_OVERLAPPED #x00000000)
(defconstant WS_CAPTION #x00C00000)
(defconstant WS_SYSMENU #x00080000)
(defconstant WS_THICKFRAME #x00040000)
(defconstant WS_MINIMIZEBOX #x00020000)
(defconstant WS_MAXIMIZEBOX #x00010000)

(defconstant WS_OVERLAPPEDWINDOW (logior WS_OVERLAPPED
                                             WS_CAPTION
                                             WS_SYSMENU
                                             WS_THICKFRAME
                                             WS_MINIMIZEBOX
                                             WS_MAXIMIZEBOX))

(defconstant CW_USEDEFAULT -2147483648)

(defconstant SW_SHOW 5)

(defconstant WM_DESTROY #x0002)

(defctype LPVOID :pointer)
(defctype WNDPROC :pointer)

(defctype HGDIOBJ :pointer)

(defctype HINSTANCE :pointer)
(defctype HMODULE HINSTANCE)

(defctype HICON :pointer)
(defctype HCURSOR :pointer)
(defctype HBRUSH :pointer)
(defctype HWND :pointer)
(defctype HMENU :pointer)

(defctype WPARAM :unsigned-int)
(defctype LPARAM :long)
(defctype LRESULT :long)

(defctype WORD :unsigned-short)
(defctype DWORD :unsigned-long)
(defctype BOOL :int)

(defctype W32-ATOM WORD)

(defctype LPCSTR :string)
(defctype LPCTSTR LPCSTR)
(defctype LPCWSTR :string)

(defcstruct tagPOINT
  (x :long)
  (y :long))

(defctype POINT   (:struct tagPOINT))
(defctype PPOINT  (:pointer POINT))
(defctype LPPOINT (:pointer POINT))

(defcstruct tagWNDCLASS
  (style         :UINT)
  (lpfnWndProc   WNDPROC)
  (cbClsExtra    :INT)
  (cbWndExtra    :INT)
  (hInstance     HINSTANCE)
  (hIcon         HICON)
  (hCursor       HCURSOR)
  (hbrBackground HBRUSH)
  (lpszMenuName  LPCWSTR)
  (lpszClassName LPCWSTR))

(defctype WNDCLASS  (:struct tagWNDCLASS))
(defctype PWNDCLASS (:pointer WNDCLASS))

(defcstruct tagMSG
  (hwnd    HWND)
  (message :UINT)
  (wParam  WPARAM)
  (lParam  LPARAM)
  (time    DWORD)
  (pt      POINT))

(defctype MSG   (:struct tagMSG))
(defctype PMSG  (:pointer MSG))
(defctype LPMSG (:pointer MSG))

(defcfun ("RegisterClassW" RegisterClass) W32-ATOM
  (lpwcx (:pointer WNDCLASS)))

(defcfun ("GetModuleHandleW" GetModuleHandle) HMODULE
  (lpModuleName lpctstr))

(defcfun ("LoadIconW" LoadIcon) HICON
  (hInstance  HINSTANCE)
  (lpIconName LPCTSTR))

(defcfun ("LoadCursorW" LoadCursor) HCURSOR
  (hInstance    HINSTANCE)
  (lpCursorName LPCTSTR))

(defcfun "GetStockObject" HGDIOBJ
  (fnObject :int))

(defcfun ("CreateWindowExW" CreateWindowEx) HWND
  (dwExStyle    DWORD)
  (lpClassName  LPCTSTR)
  (lpWindowName LPCTSTR)
  (dwStyle      DWORD)
  (x            :INT)
  (y            :INT)
  (nWidth       :INT)
  (nHeight      :INT)
  (hWndParent   HWND)
  (hMenu        HMENU)
  (hInstance    HINSTANCE)
  (lpParam      LPVOID))

(defcfun "ShowWindow" BOOL
  (hWnd HWND)
  (nCmdShow :INT))

(defcfun "UpdateWindow" BOOL
  (hWnd HWND))

(defcfun ("GetMessageW" GetMessage) BOOL
  (lpMsg         LPMSG)
  (hWnd          HWND)
  (wMsgFilterMin :UINT)
  (wMsgFilterMax :UINT))

(defcfun "TranslateMessage" BOOL
  (lpMsg LPMSG))

(defcfun ("DispatchMessageW" DispatchMessage) LRESULT
  (lpmsg LPMSG))

(defcfun "PostQuitMessage" :void (nExitCode :int))

(defcfun ("DefWindowProcW" DefWindowProc) LRESULT
  (hWnd   HWND)
  (msg    :UINT)
  (wparam WPARAM)
  (lparam LPARAM))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (with-foreign-strings ((wndClassName "SimpleWin")
                         (wndTitle "Simple Window"))
                        (with-foreign-object (wc 'WNDCLASS)
                                             (setf (foreign-slot-value wc 'WNDCLASS 'style)         (logior CS_HREDRAW CS_VREDRAW)
                                                   (foreign-slot-value wc 'WNDCLASS 'lpfnWndProc)   (callback WindowFunc)
                                                   (foreign-slot-value wc 'WNDCLASS 'cbClsExtra)    0
                                                   (foreign-slot-value wc 'WNDCLASS 'cbWndExtra)    0
                                                   (foreign-slot-value wc 'WNDCLASS 'hInstance)     (GetModuleHandle (null-pointer))
                                                   (foreign-slot-value wc 'WNDCLASS 'hIcon)         (LoadIcon (null-pointer)
                                                                                                              IDI_APPLICATION)
                                                   (foreign-slot-value wc 'WNDCLASS 'hCursor)       (LoadCursor (null-pointer)
                                                                                                                IDC_ARROW)
                                                   (foreign-slot-value wc 'WNDCLASS 'hbrBackground) (GetStockObject WHITE_BRUSH)
                                                   (foreign-slot-value wc 'WNDCLASS 'lpszMenuName)  (null-pointer)
                                                   (foreign-slot-value wc 'WNDCLASS 'lpszClassName) wndClassName)

                                             (RegisterClass wc))

                        (let ((hwnd (CreateWindowEx 0
                                                    wndClassName
                                                    wndTitle
                                                    WS_OVERLAPPEDWINDOW
                                                    CW_USEDEFAULT
                                                    CW_USEDEFAULT
                                                    CW_USEDEFAULT
                                                    CW_USEDEFAULT
                                                    (null-pointer)
                                                    (null-pointer)
                                                    (GetModuleHandle (null-pointer))
                                                    (null-pointer))))
                          (ShowWindow hwnd SW_SHOW)
                          (UpdateWindow hwnd)

                          (with-foreign-object (msg 'MSG)
                                               (loop with result
                                                     while (not (= 0 (setf result (GetMessage MSG (null-pointer) 0 0))))
                                                     do (progn
                                                          (TranslateMessage msg)
                                                          (DispatchMessage msg)))
                                               (foreign-slot-value msg 'MSG 'wParam)))))
