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

(defpackage :Lunette.Examples.poppad1
  (:use #:common-lisp
        #:cffi
        #:Lunette)
  (:import-from :Lunette.System
                #:loword
                #:hiword)
  (:import-from :Lunette.Graphics
                #:RECT
                #:left
                #:top
                #:right
                #:bottom)
  (:import-from :Lunette.Controls.Edit
                #:ES_LEFT
                #:ES_MULTILINE
                #:ES_AUTOHSCROLL
                #:ES_AUTOVSCROLL

                #:EN_ERRSPACE
                #:EN_MAXTEXT)
  (:import-from :Lunette.Messages
                #:WM_SIZE
                #:WM_CREATE
                #:WM_SETFOCUS
                #:WM_COMMAND)
  (:import-from :Lunette.Dialogs.MessageBox
                #:MB_OK
                #:MB_ICONSTOP

                #:message-box)
  (:import-from :Lunette.Windows
                #:WS_CHILD
                #:WS_VISIBLE
                #:WS_HSCROLL
                #:WS_VSCROLL
                #:WS_BORDER

                #:MoveWindow
                #:SetFocus
                #:GetClientRect)
    (:export #:winmain))
