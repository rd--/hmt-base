{-
GLU  = <https://www.khronos.org/registry/OpenGL/specs/gl/glu1.3.pdf>
GLUT = <https://www.opengl.org/resources/libraries/glut/glut-3.spec.pdf>
-}

import qualified Control.Monad {- base -}
import qualified Data.Fixed {- base -}
import qualified Data.IORef {- base -}
import qualified Data.List.Split {- base -}
import qualified System.Exit {- base -}
import Text.Printf {- base -}

import System.FilePath {- filepath -}

import Graphics.UI.GLUT as Glut {- GLUT -}

import qualified Data.IntMap.Strict as Map {- containers -}

import qualified Music.Theory.Geometry.Obj as Obj {- hmt-base -}
import qualified Music.Theory.Geometry.Off as Off {- hmt-base -}
import qualified Music.Theory.Geometry.Vector as Vector {- hmt-base -}
import qualified Music.Theory.Graph.Type as Graph {- hmt-base -}
import qualified Music.Theory.Opt as Opt {- hmt-base -}

-- * Geometry

-- | R=Real
type R = Glut.GLfloat
type R3 = Vector.V3 R
type R4 = Vector.V4 R

-- * Gr/Obj-Off

-- | Gr=Graph
type Gr = Graph.Lbl R4 ()

f64_to_r :: Double -> R
f64_to_r = realToFrac

-- | If Obj file has no edges and if CH is true then make edges for all adjacent vertices.
--   If CH is true and there are edges, CH is ignored, allowing mixed sets to be loaded by setting CH.
obj_load :: Bool -> FilePath -> IO Gr
obj_load ch fn = do
  (v,e) <- Obj.obj_load_lbl_ fn
  let f (i,(x,y,z)) = (i,(f64_to_r x,f64_to_r y,f64_to_r z,0))
  case (ch,null e) of
    (True,True) -> return (map f v,zip (map (\i -> (i,i + 1)) [0 .. length v - 2]) (repeat ()))
    (False,True) -> error "obj_load?"
    (_,False) -> return (map f v,e)

off_load :: FilePath -> IO Gr
off_load =
  let f (x,y,z) = (x,y,z,0)
  in fmap Off.off_graph . Off.off_load_either (f,id)

obj_off_load :: Bool -> FilePath -> IO Gr
obj_off_load ch fn =
  case takeExtension fn of
    ".obj" -> obj_load ch fn
    ".off" -> if ch then error "CH AT Off?" else off_load fn
    _ -> error "obj_off_load: EXT?"

gr_to_vsq :: Gr -> [R4]
gr_to_vsq (v,e) =
  let m = Map.fromAscList v
      ix = maybe (error "?") id . flip Map.lookup m
      f ((i,j),_) = [ix i,ix j]
  in concatMap f e

type Ln = [[R4]]

gr_load_set :: (Bool,Bool) -> [FilePath] -> IO Ln
gr_load_set (ch,nrm) fn = do
  v <- mapM (fmap gr_to_vsq . obj_off_load ch) fn
  let c = (if nrm then Vector.v4_linlin_set (-1,1) else id) (concat v)
  return (Data.List.Split.chunksOf 16 c)  -- does sending vertices in chunks help?

-- * IoRef

withIORef :: Data.IORef.IORef a -> (a -> IO ()) -> IO ()
withIORef s f = Data.IORef.readIORef s >>= f

-- * State

-- | (zoom,rotation-(x,y,z),translation-(x,y,z),osd,prj)
type State = (R,R3,R3,Bool,(String,R4 -> R3))

-- | State Reference
type State_Ref = Data.IORef.IORef State

state_0 :: State
state_0 = (1.0,(20,30,0),(0,0,0),False,("XYZ",Vector.v4_xyz))

mod_state :: State_Ref -> (State -> State) -> IO ()
mod_state s = Data.IORef.modifyIORef s

mod_osd :: State_Ref -> IO ()
mod_osd s = mod_state s (\(z,t,r,o,p) -> (z,t,r,not o,p))

mod_trs_f :: R3 -> State -> State
mod_trs_f (dx,dy,dz) (s,r,(x,y,z),o,p) = (s,r,(x + dx,y + dy,z + dz),o,p)

mod_trs :: State_Ref -> R3 -> IO ()
mod_trs s d = mod_state s (mod_trs_f d)

mod_rot_f :: R3 -> State -> State
mod_rot_f (dx,dy,dz) (s,(x,y,z),t,o,p) =
  let f i j = Data.Fixed.mod' (i + j) 360
  in (s,(f x dx,f y dy,f z dz),t,o,p)

mod_rot :: State_Ref -> R3 -> IO ()
mod_rot s d = mod_state s (mod_rot_f d)

set_rot :: State_Ref -> R3 -> IO ()
set_rot s rt = mod_state s (\(sc,_,tr,o,p) -> (sc,rt,tr,o,p))

mod_zoom_f :: R -> State -> State
mod_zoom_f n (s,r,t,o,p) = (s + n,r,t,o,p)

mod_zoom :: State_Ref -> R -> IO ()
mod_zoom s n = mod_state s (mod_zoom_f n)

set_init :: State_Ref -> IO ()
set_init s = mod_state s (const state_0)

set_prj_f :: (String,R4 -> R3) -> State -> State
set_prj_f p (s,r,t,o,_) = (s,r,t,o,p)

set_prj :: State_Ref -> (String,R4 -> R3) -> IO ()
set_prj s p = mod_state s (set_prj_f p)

-- * Gl

{-
gl_with_time :: String -> IO x -> IO ()
gl_with_time m x = do
  t0 <- elapsedTime
  _ <- x
  t1 <- elapsedTime
  print (m,t1 - t0)
-}

gl_render_ln :: State -> Ln -> IO ()
gl_render_ln (_,_,_,_,(_,p)) =
  let v (x,y,z) = Vertex3 x y z
      f i = renderPrimitive Lines (mapM_ (vertex . v . p) i)
  in mapM_ f

gl_grey :: Glut.Color4 R
gl_grey = Color4 0.5 0.5 0.5 0.5

gl_render_state :: State -> IO ()
gl_render_state (sc,(rx,ry,rz),(tx,ty,tz),_,_) = do
    translate (Vector3 tx ty tz)
    rotate rx (Vector3 1 0 0)
    rotate ry (Vector3 0 1 0)
    rotate rz (Vector3 0 0 1)
    scale sc sc sc
    color gl_grey

r_to_int :: R -> Int
r_to_int = round

state_pp :: State -> String
state_pp (sc,(rx,ry,rz),(tx,ty,tz),_,(p,_)) =
  let i n = r_to_int (if n > 180 then n - 360 else n)
  in printf "%.2f (%d,%d,%d) (%.1f,%.1f,%.1f) %s" sc (i rx) (i ry) (i rz) tx ty tz p

gl_render_txt :: State -> IO ()
gl_render_txt st = do
  let (_,_,_,o,_) = st
  Control.Monad.when o (rasterPos (Vertex3 (- 2.5) (-2.5) 0 :: Vertex3 R) >>
          renderString Fixed8By13 (state_pp st))

gl_draw :: Ln -> State -> IO ()
gl_draw ln s = do
  clear [ColorBuffer]
  preservingMatrix (gl_render_txt s >> gl_render_state s >> gl_render_ln s ln)
  swapBuffers

gl_keydown :: State_Ref -> Glut.Key -> Glut.Modifiers -> IO ()
gl_keydown s ky m = do
  let a = alt m == Down
      c = ctrl m == Down
      r = 5
  case ky of
    SpecialKey KeyDown -> if c then mod_trs s (0,-0.1,0) else mod_rot s (- r,0,0)
    SpecialKey KeyUp -> if c then mod_trs s (0,0.1,0) else mod_rot s (r,0,0)
    SpecialKey KeyLeft -> if c then mod_trs s (-0.1,0,0) else mod_rot s (0,- r,0)
    SpecialKey KeyRight -> if c then mod_trs s (0.1,0,0) else mod_rot s (0,r,0)
    SpecialKey KeyPageUp -> if c then mod_trs s (0,0,0.1) else mod_rot s (0,0,r)
    SpecialKey KeyPageDown -> if c then mod_trs s (0,0,-0.1) else mod_rot s (0,0,- r)
    SpecialKey KeyF1 -> set_prj s ("XYZ",Vector.v4_xyz)
    SpecialKey KeyF2 -> set_prj s ("XYW",Vector.v4_xyw)
    SpecialKey KeyF3 -> set_prj s ("XZW",Vector.v4_xzw)
    SpecialKey KeyF4 -> set_prj s ("YZW",Vector.v4_yzw)
    Char '=' -> mod_zoom s (if c then 0.1 else 0.01)
    Char '-' -> mod_zoom s (if c then -0.1 else -0.01)
    Char '1' -> set_rot s (if not a then (0,0,0) else (0,180,0)) -- Y
    Char '2' -> set_rot s (if not a then (90,0,0) else (270,0,0)) -- X
    Char '3' -> set_rot s (if not a then (90,0,90) else (270,0,270)) -- X/Z
    Char '4' -> set_rot s (if not a then (0,0,90) else (0,0,270)) -- Z
    Char '5' -> set_rot s (if not a then (90,0,180) else (90,180,0)) -- Y/Z
    Char '0' -> set_init s
    Char 'o' -> mod_osd s
    Char 'Q' -> System.Exit.exitWith System.Exit.ExitSuccess
    _ -> return ()

gl_keyboard :: State_Ref -> Glut.Key -> Glut.KeyState -> Glut.Modifiers -> Glut.Position -> IO ()
gl_keyboard s ky ks m _ = if ks == Down then gl_keydown s ky m else return ()

gl_init :: IO ()
gl_init = do
  viewport $= (Position 0 0,Size 800 800)
  matrixMode $= Projection
  loadIdentity
  perspective 30 1 1 100
  matrixMode $= Modelview 0
  loadIdentity
  lookAt (Vertex3 0 0 10) (Vertex3 0 0 0) (Vector3 0 1 0)
  shadeModel $= Smooth
  clearColor $= Color4 0 0 0 0
  blend $= Enabled
  blendFunc $= (SrcAlpha,One)
  depthFunc $= Nothing
  lighting $= Disabled
  normalize $= Enabled

timer_f :: Glut.Timeout -> IO ()
timer_f dly = do
  postRedisplay Nothing
  addTimerCallback dly (timer_f dly)

gl_gr_obj :: (Bool,Bool) -> Glut.GLsizei -> Glut.Timeout -> [FilePath] -> IO ()
gl_gr_obj opt sz dly fn = do
  ln <- gr_load_set opt fn
  _ <- initialize "Gr-Obj-Off" []
  initialDisplayMode $= [RGBAMode,DoubleBuffered]
  initialWindowSize $= Size sz sz
  initialWindowPosition $= Position 0 0
  _ <- createWindow "Gl"
  gl_init
  s <- Data.IORef.newIORef state_0
  displayCallback $= withIORef s (gl_draw ln)
  keyboardMouseCallback $= Just (gl_keyboard s)
  addTimerCallback dly (timer_f dly)
  mainLoop

cli_usg :: Opt.OptHelp
cli_usg = ["obj-gr [opt] file-name..."]

cli_opt :: [Opt.OptUsr]
cli_opt =
  [("chain","False","bool","Obj is vertex sequence")
  ,("delay","100","int","timer delay (ms)")
  ,("normalise","False","bool","normalise vertex data to (-1,1)")
  ,("size","400","int","window size (px)")]

main :: IO ()
main = do
  (o,a) <- Opt.opt_get_arg True cli_usg cli_opt
  case a of
    "obj-gr":fn -> gl_gr_obj
                   (Opt.opt_read o "chain",Opt.opt_read o "normalise")
                   (Opt.opt_read o "size")
                   (Opt.opt_read o "delay")
                   fn
    _ -> Opt.opt_usage cli_usg cli_opt

