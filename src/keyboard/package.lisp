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

(defpackage :Lunette.Examples.typer
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.System
                #:hiword
                #:loword)
  (:import-from :Lunette.Messages
                #:WM_INPUTLANGCHANGE
                #:WM_CREATE
                #:WM_SIZE
                #:WM_SETFOCUS
                #:WM_KILLFOCUS
                #:WM_KEYDOWN
                #:WM_CHAR

                #:SendMessage)
  (:import-from :Lunette.VirtualKeys
                #:VK_HOME
                #:VK_END
                #:VK_PRIOR
                #:VK_NEXT
                #:VK_LEFT
                #:VK_RIGHT
                #:VK_UP
                #:VK_DOWN
                #:VK_DELETE)
  (:import-from :Lunette.Graphics
                #:SYSTEM_FONT

                #:RECT
                #:left
                #:top
                #:right
                #:bottom

                #:GetStockObject
                #:SelectObject
                #:DeleteObject
                #:with-dc
                #:InvalidateRect)
  (:import-from :Lunette.Windows
                #:GetClientRect
                #:GetFocus)
  (:import-from :Lunette.Graphics.Text
                #:FW_DONTCARE
                #:DEFAULT_CHARSET
                #:FF_DONTCARE
                #:FIXED_PITCH

                #:TEXTMETRIC
                #:tmAveCharWidth
                #:tmHeight
                #:PTEXTMETRIC

                #:text-out
                #:GetTextMetrics
                #:CreateFont
                #:create-font
                #:CreateCaret
                #:SetCaretPos
                #:ShowCaret
                #:HideCaret
                #:DestroyCaret)
  (:export #:winmain))
