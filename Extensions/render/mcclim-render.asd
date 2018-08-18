(cl:in-package #:asdf-user)

(defsystem #:mcclim-render
  :description "Support for raster images McCLIM."
  :depends-on (#:mcclim-render/core
               #:mcclim-render/two-dim-array))

(defsystem #:mcclim-render/core
  :depends-on (#:clim-basic
               #:mcclim-image
               #:mcclim-fonts/truetype)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "color")
               (:file "image")
               (:file "image-prim")
               (:file "pixeled-design")
               (:file "recording")))

(defsystem #:mcclim-render/render
    :depends-on (#:mcclim-render/core #:cl-vectors)
    :components
    ((:module "render"
              :serial t
              :components
              ((:file "prim-arc")
               (:file "prim-text")))))

(defsystem #:mcclim-render/cl-vectors
  :depends-on (#:clim-basic
               #:mcclim-fonts/truetype
               #:mcclim-render/render
               #:mcclim-render/two-dim-array)
    :components
    ((:module "cl-vectors"
              :serial t
              :components
              ((:file "vectors")
               (:file "vectors-image-ops")))))

(defsystem #:mcclim-render/two-dim-array
  :depends-on (#:mcclim-render/core)
  :serial t
  :components ((:module "two-dim-array"
                        :serial t
                        :components
                        ((:file "two-dim-array-image")))))

(defsystem #:mcclim-render/backend
    :depends-on (#:mcclim-render/cl-vectors)
    :components
    ((:module "backend"
              :serial t
              :components
              ((:file "mirror")
               (:file "mirrored-sheet")
               (:file "pixmap")
               (:file "medium")
               (:file "fonts")
               (:file "port")))))

(defsystem #:mcclim-render/clx
    :depends-on (#:mcclim-clx
                 #:mcclim-render/backend
                 #:mcclim-render/two-dim-array))
