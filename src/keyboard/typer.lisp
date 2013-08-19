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

(in-package :Lunette.Examples.typer)

(let ((pBuffer)
      (bWidth)
      (bHeight))

  (defun getOffset (x y)
    (+ x (* y bWidth)))

  (defun setBufferSize (width height)
    (setf bWidth width
          bHeight height)

    (setf pBuffer (make-string (* bWidth bHeight) :initial-element #\Space)))

  (defun deleteChar (xPos yPos)
    (loop for i
          from xPos
          to (1- bWidth)
          do (setf (char pBuffer (getOffset i yPos))
                   (char pBuffer (getOffset (1+ i) yPos))))

    (setf (char pBuffer (getOffset (1- bWidth) yPos)) #\Space))

  (defun getLine (xPos yPos)
    (let ((str (make-string (- bWidth xPos))))
      (loop for i
            from 0
            to (1- (- bWidth xPos))
            do (setf (char str i)
                     (char pBuffer (getOffset (+ i xPos) yPos))))
      str))

  (defun clearBuffer ()
    (loop for i
          from 0
          to (1- (* bWdith bHeight))
          do (setf (char pBuffer i) #\Space)))

  (defun setCharAt (xPos yPos aChar)
    (setf (char pBuffer (getOffset xPos yPos)) aChar)))

(let ((dwCharSet DEFAULT_CHARSET)
      (cxChar)
      (cyChar)
      (cxClient)
      (cyClient)
      (cxBuffer)
      (cyBuffer))

  (defun handleWinSize (hWnd width height)
    (when (and width height)
      (setf cxClient width
            cyClient height))

    (setf cxBuffer (max 1 (ceiling (/ cxClient cxChar)))
          cyBuffer (max 1 (ceiling (/ cyClient cyChar))))

    (setBufferSize cxBuffer cyBuffer)

    (setf xCaret 0
          yCaret 0)

    (when (eql hWnd (GetFocus))
      (SetCaretPos (* xCaret cxChar)
                   (* yCaret cyChar)))

    (InvalidateRect hWnd (null-pointer) 1))

  (defun handleWinCreate (hWnd)
    (with-dc hdc hWnd
             (SelectObject hdc (create-font :fdwCharSet dwCharSet
                                            :fdwPitchAndFamily (logior FF_DONTCARE
                                                                       FIXED_PITCH)))

             (with-foreign-object (tm 'TEXTMETRIC)
                                  (GetTextMetrics hdc tm)
                                  (setf cxChar (foreign-slot-value tm 'TEXTMETRIC 'tmAveCharWidth)
                                        cyChar (foreign-slot-value tm 'TEXTMETRIC 'tmHeight)))

             (DeleteObject (SelectObject hdc (GetStockObject SYSTEM_FONT))))

    (with-foreign-object (rect 'RECT)
                         (GetClientRect hWnd rect)
                         (setf cxClient (- (foreign-slot-value rect 'RECT 'right)
                                           (foreign-slot-value rect 'RECT 'left))
                               cyClient (- (foreign-slot-value rect 'RECT 'bottom)
                                           (foreign-slot-value rect 'RECT 'top))))

    (handleWinSize hWnd nil nil))

  (defun handleLangChange (hwnd newCharset)
    (setf dwCharSet newCharset)

    (handleWinCreate hwnd))

  (defun handleSetFocus (hWnd)
    (CreateCaret hWnd (null-pointer) cxChar cyChar)
    (SetCaretPos (* xCaret cxChar)
                 (* yCaret cyChar))
    (ShowCaret hWnd))

  (defun handleKillFocus (hWnd)
    (HideCaret hWnd)
    (DestroyCaret))

  (defun handleKey (hWnd vkey)
    (cond
     ((eql vkey VK_HOME)
      (setf xCaret 0))
     ((eql vkey VK_END)
      (setf xCaret (1- cxBuffer)))
     ((eql vkey VK_PRIOR)
      (setf yCaret 0))
     ((eql vkey VK_NEXT)
      (setf yCaret (1- cyBuffer)))
     ((eql vkey VK_LEFT)
      (setf xCaret (max (1- xCaret) 0)))
     ((eql vkey VK_RIGHT)
      (setf xCaret (min (1+ xCaret) (1- cxBuffer))))
     ((eql vkey VK_UP)
      (setf yCaret (max (1- yCaret) 0)))
     ((eql vkey VK_DOWN)
      (setf yCaret (min (1+ yCaret) (1- cyBuffer))))
     ((eql vkey VK_DELETE)
      (deleteChar xCaret yCaret)

      (HideCaret hWnd)
      (with-dc hdc hWnd
               (SelectObject hdc (create-font :fdwCharSet dwCharSet
                                              :fdwPitchAndFamily (logior FF_DONTCARE
                                                                         FIXED_PITCH)))

               (text-out hdc (* xCaret cxChar) (* yCaret cyChar) (getLine xCaret yCaret))

               (DeleteObject (SelectObject hdc (GetStockObject SYSTEM_FONT))))
      (ShowCaret hWnd)))

    (SetCaretPos (* xCaret cxChar)
                 (* yCaret cyChar)))

  (defun handleChar (hWnd aChar)
    (cond
     ((eql aChar #\Backspace)
      (when (> 0 xCaret)
        (decf xCaret)
        (SendMessage hWnd WM_KEYDOWN VK_DELETE 1)))
     ((eql aChar #\Tab)
      (loop do (SendMessage hWnd WM_CHAR (char-code #\Space) 1)
            until (= 0 (mod xCaret 4))))
     ((eql aChar #\Linefeed)
      (when (= cyBuffer (incf yCaret))
        (setf yCaret 0)))
     ((eql aChar #\Return)
      (setf xCaret 0)
      (when (= cyBuffer (incf yCaret))
        (setf yCaret 0)))
     ((eql aChar #\Esc)
      (clearBuffer)
      (setf xCaret 0
            yCaret 0)
      (InvalidateRect hWnd (null-pointer) 0))
     (t (setCharAt xCaret yCaret aChar)

        (HideCaret hWnd)
        (with-dc hdc hWnd
                 (SelectObject hdc (create-font :fdwCharSet dwCharSet
                                                :fdwPitchAndFamily (logior FF_DONTCARE
                                                                           FIXED_PITCH)))

                 (text-out hdc (* xCaret cxChar) (* yCaret cyChar) (getLine xCaret yCaret))

                 (DeleteObject (SelectObject hdc (GetStockObject SYSTEM_FONT))))
        (ShowCaret hWnd)

        (when (= cxBuffer (incf xCaret))
          (setf xCaret 0)

          (when (= cyBuffer (incf yCaret))
            (setf yCaret 0)))))

    (SetCaretPos (* xCaret cxChar)
                 (* yCaret cyChar)))

  (defun handlePaint (hWnd hdc)
    (SelectObject hdc (create-font :fdwCharSet dwCharSet
                                   :fdwPitchAndFamily (logior FF_DONTCARE
                                                              FIXED_PITCH)))

    (loop for i
          from 0
          to (1- cyBuffer)
          do (text-out hdc 0 (* i cyChar) (getLine 0 i)))

    (DeleteObject (SelectObject hdc (GetStockObject SYSTEM_FONT)))))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
   ((eql msg WM_INPUTLANGCHANGE)
    (handleLangChange hWnd wparam)
    0)
   ((eql msg WM_CREATE)
    (handleWinCreate hWnd)
    0)
   ((eql msg WM_SIZE)
    (handleWinSize hWnd (loword lparam) (hiword lparam))
    0)
   ((eql msg WM_SETFOCUS)
    (handleSetFocus hWnd)
    0)
   ((eql msg WM_KILLFOCUS)
    (handleKillFocus hWnd)
    0)
   ((eql msg WM_KEYDOWN)
    (handleKey hWnd wparam)
    0)
   ((eql msg WM_CHAR)
    (loop repeat (loword lparam)
          do (handleChar hWnd (code-char wParam)))
    0)
   ((eql msg WM_PAINT)
    (with-ps ps hdc hWnd
             (handlePaint hWnd hdc))
    0)
   ((eql msg WM_DESTROY)
    (PostQuitMessage 0)
    0)
   (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "TyperWin" :lpfnWndProc (callback WindowFunc))

  (let ((hwnd (create-window-ex "TyperWin" "Typer")))
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
