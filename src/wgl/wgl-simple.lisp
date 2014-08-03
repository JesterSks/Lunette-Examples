#|
   Copyright 2014 Robert Burghart

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

(in-package :Lunette.Examples.wgl)

(defparameter wgl-hdc nil)
(defparameter wgl-hrc nil)

(defun SetDCPixelFormat (hdc)
  (with-foreign-object (pfd 'PIXELFORMATDESCRIPTOR)
    (setf (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'nSize)                (foreign-type-size 'PIXELFORMATDESCRIPTOR)
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'nVersion)             1
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'dwFlags)              (logior PFD_DRAW_TO_WINDOW
                                                                                        PFD_SUPPORT_OPENGL
                                                                                        PFD_DOUBLEBUFFER)
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'iPixelType)           PFD_TYPE_RGBA
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cColorBits)           32
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cRedBits)             0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cRedShift)            0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cGreenBits)           0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cGreenShift)          0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cBlueBits)            0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cBlueShift)           0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cAlphaBits)           0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cAlphaShift)          0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cAccumBits)           0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cAccumRedBits)        0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cAccumGreenBits)      0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cAccumBlueBits)       0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cAccumAlphaBits)      0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cDepthBits)           16
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cStencilBits)         0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'cAuxBuffers)          0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'iLayerType)           0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'bReserved)            0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'dwLayerMask)          0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'dwVisibleMask)        0
          (foreign-slot-value pfd 'PIXELFORMATDESCRIPTOR 'dwDamageMask)         0)
    (let ((nPixelFormat (ChoosePixelFormat hdc pfd)))
      (SetPixelFormat hdc nPixelFormat pfd))))

(defun RenderScene ()
  (gl:clear :color-buffer-bit)
  (gl:color 1.0 0.0 0.0)
  (gl:rect -25.0 25.0 25.0 -25.0)
  (gl:flush))

(defun ChangeSize (w h)
  (when (= h 0)
    (setf h 1))

  (gl:viewport 0 0 w h)

  (gl:matrix-mode :projection)
  (gl:load-identity)

  (let ((fAspect (/ w h)))
    (if (<= w h)
        (let ((winW 100)
              (winH (/ 100.0 fAspect)))
          (gl:ortho -100.0 100.0 (- winH) winH 1.0 -1.0))
        (let ((winW (* 100.0 fAspect))
              (winH 100.0))
          (gl:ortho (- winW) winW -100.0 100.0 1.0 -1.0))))

  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defcallback WindowFunc LRESULT ((hWnd HWND)
                                 (msg :UINT)
                                 (wparam WPARAM)
                                 (lparam LPARAM))
  (cond
    ((eql msg WM_CREATE)
     (setf wgl-hdc (GetDC hWnd))
     (SetDCPixelFormat wgl-hdc)
     (setf wgl-hrc (wglCreateContext wgl-hdc))
     (wglMakeCurrent wgl-hdc wgl-hrc)
     0)
    ((eql msg WM_PAINT)
     (RenderScene)
     (SwapBuffers wgl-hdc)
     0)
    ((eql msg WM_SIZE)
     (ChangeSize (loword lParam) (hiword lParam))
     0)
    ((eql msg WM_DESTROY)
     (wglMakeCurrent (null-pointer) (null-pointer))
     (wglDeleteContext wgl-hrc)

     (PostQuitMessage 0)
     0)
    (t (DefWindowProc hwnd msg wparam lparam))))

(defun WinMain ()
  (setf *default-foreign-encoding* :utf-16le)

  (register-class "OpenGLWin" :lpfnWndProc (callback WindowFunc)
                  :style (logior CS_HREDRAW CS_VREDRAW CS_OWNDC))

  (let ((hwnd (create-window-ex "OpenGLWin" "OpenGL"
                                :dwStyle (logior WS_OVERLAPPEDWINDOW
                                                 WS_CLIPCHILDREN
                                                 WS_CLIPSIBLINGS))))
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
