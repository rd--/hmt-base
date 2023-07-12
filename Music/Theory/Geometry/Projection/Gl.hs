module Music.Theory.Geometry.Projection.Gl where

import Music.Theory.Geometry.Matrix {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}

-- | <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glOrtho.xml>
ortho :: (Fractional n) => V4 n -> V2 n -> M44 n
ortho (left,right,bottom,top) (nearval,farval) =
  ((2 / (right-left),0,0,0)
  ,(0,2 / (top-bottom),0,0)
  ,(0,0,-2 / (farval-nearval),0)
  ,(-(right+left) / (right-left),-(top+bottom) / (top-bottom),-(farval+nearval) / (farval-nearval),1))

-- | <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFrustum.xml>
frustum :: (Fractional n) => V2 n -> V2 n -> V2 n -> M44 n
frustum (left,right) (bottom,top) (nearval,farval) =
  let x = (2*nearval) / (right-left)
      y = (2*nearval) / (top-bottom)
      a = (right+left) / (right-left)
      b = (top+bottom) / (top-bottom)
      c = -(farval+nearval) / (farval-nearval)
      d = -(2*farval*nearval) / (farval-nearval)  -- error?
  in ((x,0,0,0),(0,y,0,0),(a,b,c,-1),(a,0,d,0))

-- | <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml>
gl_perspective :: (Floating n) => V4 n -> M44 n
gl_perspective (fovy,aspect,zNear,zFar) =
  let ymax = zNear * tan (fovy * pi / 360)
      ymin = -ymax
      xmin = ymin * aspect
      xmax = ymax * aspect
  in frustum (xmin,xmax) (ymin,ymax) (zNear,zFar)

{- | <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluLookAt.xml>

>>> lookAt (0,0,10) (0,0,0) (0,1,0)
((1.0,0.0,0.0,-0.0),(0.0,-1.0,0.0,-0.0),(0.0,0.0,1.0,-10.0),(0.0,0.0,0.0,1.0))
-}
lookAt :: (Floating t) => V3 t -> V3 t -> V3 t -> M44 t
lookAt (eyex,eyey,eyez) (centerx,centery,centerz) (upx,upy,upz) =
   let z = v3_normalize (eyex - centerx,eyey - centery,eyez - centerz)
       y = (upx,upy,upz)
       x = v3_cross_product y z
       y' = v3_cross_product z x
       (x0,x1,x2) = v3_normalize x
       (y0,y1,y2) = v3_normalize y'
       (z0,z1,z2) = z
   in ((x0,y0,z0,-eyex),(x1,y1,z1,-eyey),(x2,y2,z2,-eyez),(0,0,0,1))

{-
infinitePerspectiveRH :: (Floating t) => V3 t -> M44 t
infinitePerspectiveRH (fovy,aspect,zNear)  =
  let range = tan(fovy / 2) * zNear
      left = -range * aspect
      right = range * aspect
      bottom = -range
      top = range
  in (((2 * zNear) / (right - left),0,0,0)
     ,(0,(2 * zNear) / (top - bottom),0,0)
     ,(0,0,-1,-1)
     ,(0,0,-2 * zNear,0))
-}
