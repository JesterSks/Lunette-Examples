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

(defpackage :Lunette.Examples.connect
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.System
                #:GetModuleHandle
                #:hiword
                #:loword)
  (:import-from :Lunette.Messages
                #:WM_MOUSEMOVE
                #:WM_LBUTTONDOWN
                #:WM_LBUTTONUP)
  (:import-from :Lunette.Graphics
                #:SetPixel
                #:MoveToEx
                #:LineTo
                #:with-dc
                #:InvalidateRect)
  (:import-from :Lunette.Resources
                #:IDC_ARROW
                #:IDC_WAIT

                #:LoadCursor)
  (:import-from :Lunette.Windows
                #:MK_LBUTTON

                #:SetCursor
                #:ShowCursor)
  (:export #:winmain))

(defpackage :Lunette.Examples.checker1
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.System
                #:hiword
                #:loword)
  (:import-from :Lunette.Errors
                #:MessageBeep)
  (:import-from :Lunette.Messages
                #:WM_SIZE
                #:WM_CREATE
                #:WM_LBUTTONDOWN)
  (:import-from :Lunette.Graphics
                #:RECT
                #:left
                #:top
                #:right
                #:bottom

                #:MoveToEx
                #:LineTo
                #:Rectangle
                #:InvalidateRect)
  (:import-from :Lunette.Windows
                #:MB_OK

                #:GetClientRect)
  (:export #:winmain))

(defpackage :Lunette.Examples.checker2
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.System
                #:POINT
                #:x
                #:y

                #:hiword
                #:loword
                #:makelong)
  (:import-from :Lunette.Errors
                #:MessageBeep)
  (:import-from :Lunette.Messages
                #:WM_SIZE
                #:WM_CREATE
                #:WM_LBUTTONDOWN
                #:WM_SETFOCUS
                #:WM_KILLFOCUS
                #:WM_KEYDOWN

                #:SendMessage)
  (:import-from :Lunette.VirtualKeys
                #:VK_HOME
                #:VK_END
                #:VK_LEFT
                #:VK_RIGHT
                #:VK_UP
                #:VK_DOWN
                #:VK_RETURN
                #:VK_SPACE)
  (:import-from :Lunette.Graphics
                #:RECT
                #:left
                #:top
                #:right
                #:bottom

                #:MoveToEx
                #:LineTo
                #:Rectangle
                #:InvalidateRect)
  (:import-from :Lunette.Windows
                #:MB_OK
                #:MK_LBUTTON

                #:GetClientRect
                #:ShowCursor
                #:GetCursorPos
                #:SetCursorPos
                #:ScreenToClient
                #:ClientToScreen)
  (:export #:winmain))

(defpackage :Lunette.Examples.checker3
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.System
                #:hiword
                #:loword)
  (:import-from :Lunette.Errors
                #:MessageBeep)
  (:import-from :Lunette.Messages
                #:WM_SIZE
                #:WM_CREATE
                #:WM_LBUTTONDOWN)
  (:import-from :Lunette.Graphics
                #:RECT
                #:left
                #:top
                #:right
                #:bottom

                #:MoveToEx
                #:LineTo
                #:Rectangle
                #:InvalidateRect)
  (:import-from :Lunette.Windows
                #:WS_CHILDWINDOW
                #:WS_VISIBLE
                #:MB_OK

                #:GetClientRect
                #:MoveWindow
                #:SetWindowLong
                #:GetWindowLong)
  (:export #:winmain))
