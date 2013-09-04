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

(defpackage :Lunette.Examples.btnlook
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.System
                #:loword
                #:hiword)
  (:import-from :Lunette.Graphics
                #:SYSTEM_FIXED_FONT
                #:TRANSPARENT

                #:RECT
                #:left
                #:top
                #:right
                #:bottom

                #:GetStockObject
                #:SelectObject
                #:SetBkMode
                #:InvalidateRect
                #:with-dc)
  (:import-from :Lunette.Controls
                #:BS_PUSHBUTTON
                #:BS_DEFPUSHBUTTON
                #:BS_CHECKBOX
                #:BS_AUTOCHECKBOX
                #:BS_RADIOBUTTON
                #:BS_3STATE
                #:BS_AUTO3STATE
                #:BS_GROUPBOX
                #:BS_AUTORADIOBUTTON
                #:BS_OWNERDRAW

                #:ScrollWindow)
  (:import-from :Lunette.Messages
                #:WM_SIZE
                #:WM_CREATE
                #:WM_DRAWITEM
                #:WM_COMMAND)
  (:import-from :Lunette.Dialogs
                #:GetDialogBaseUnits)
  (:import-from :Lunette.Graphics.Text
                #:text-out)
  (:import-from :Lunette.Windows
                #:WS_CHILD
                #:WS_VISIBLE

                #:GetClientRect
                #:ValidateRect)
    (:export #:winmain))
