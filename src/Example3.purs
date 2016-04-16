module Example3 where

import Prelude --(Unit, (*), (/), bind, negate, ($), (+), return, (-), (<<<), liftM1, unit, map)
import Graphics.WebGLAll (EffWebGL, Buffer, Mat4, Uniform, Vec3, Attribute, WebGLProg, WebGLContext, Capacity(DEPTH_TEST),
                        Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mode(..), Shaders(Shaders),
                        drawArr, bindBufAndSetVertexAttr, setUniformFloats, clear, viewport, getCanvasHeight, getCanvasWidth,
                        requestAnimationFrame, enable, clearColor, makeBufferFloat, withShaders, runWebGL)
import Data.Matrix4 (identity, translate, rotate, makePerspective) as M
import Data.Matrix (toArray) as M
import Data.Vector3 as V3
import Control.Monad.Eff.Alert (Alert, alert)
import Data.ArrayBuffer.Types as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Date (Now, now, toEpochMilliseconds)
import Data.Time (Milliseconds(Milliseconds))
import Data.Maybe (Maybe(Just, Nothing))
import Math (pi, sin, cos, abs)
import Data.Int (toNumber)
import Data.Array
import Data.Foldable
import Data.Tuple
import Debug.Trace
import Data.Ord (comparing)

shaders :: Shaders {aVertexPosition :: Attribute Vec3, aVertexColor :: Attribute Vec3,
                      uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4}
shaders = Shaders

  """precision mediump float;

  varying vec4 vColor;

  void main(void) {
    gl_FragColor = vColor;
      }
  """

  """
      attribute vec3 aVertexPosition;
      attribute vec4 aVertexColor;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;

      varying vec4 vColor;

      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
          vColor = aVertexColor;
      }
  """

type AnimatedNumber = Tuple Number Number
type State = {
                context :: WebGLContext,
                shaderProgram :: WebGLProg,
                aVertexPosition :: Attribute Vec3,
                aVertexColor  :: Attribute Vec3,
                uPMatrix :: Uniform Mat4,
                uMVMatrix :: Uniform Mat4,
                buf1 :: Buffer T.Float32,
                buf1Colors :: Buffer T.Float32,
                buf2 :: Buffer T.Float32,
                buf2Colors :: Buffer T.Float32,
                lastTime :: Maybe Number,
                rTri :: Number,
                rSquare :: Number,
                angles :: Array AnimatedNumber,
                remainingShapes :: Array Shape
            }

type Shape = {
  name :: String,
  angles :: Array Number
}

main :: Eff (console :: CONSOLE, alert :: Alert, now :: Now) Unit
main =
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        log "WebGL started"
        withShaders shaders
                    (\s -> alert s)
                      \ bindings -> do
          buf1 <- makeBufferFloat [0.0,  1.0,  0.0,
                              (-1.0), (-1.0),  0.0,
                              1.0, (-1.0),  0.0]
          buf1Colors <- makeBufferFloat  [
                              1.0, 0.0, 0.0, 1.0,
                              0.0, 1.0, 0.0, 1.0,
                              0.0, 0.0, 1.0, 1.0
                              ]
          buf2 <- makeBufferFloat [1.0,  1.0,  0.0,
                             (-1.0), 1.0,  0.0,
                              1.0, (-1.0),  0.0,
                             (-1.0), (-1.0),  0.0]
          buf2Colors <- makeBufferFloat
                             [0.5, 0.5, 1.0, 1.0,
                             0.5, 0.5, 1.0, 1.0,
                             0.5, 0.5, 1.0, 1.0,
                             0.5, 0.5, 1.0, 1.0]
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          let state = {
                        context : context,
                        shaderProgram : bindings.webGLProgram,
                        aVertexPosition : bindings.aVertexPosition,
                        aVertexColor : bindings.aVertexColor,
                        uPMatrix : bindings.uPMatrix,
                        uMVMatrix : bindings.uMVMatrix,
                        buf1 : buf1,
                        buf1Colors : buf1Colors,
                        buf2 : buf2,
                        buf2Colors : buf2Colors,
                        lastTime : Nothing,
                        rTri : 0.0,
                        rSquare : 0.0,
                        angles: [Tuple 0.0 0.0, Tuple 0.0 90.0, Tuple 120.0 180.0, Tuple 240.0 270.0],
                        remainingShapes: [
                          mkShape "Square" $ regularAngles 4,
                          mkShape "Octagon" $ regularAngles 8,
                          mkShape "Pentagon" $ regularAngles 5,
                          mkShape "Nonagon" $ regularAngles 9,
                          mkShape "Equilateral Triangle" $ regularAngles 3,
                          mkShape "Hexagon" $ regularAngles 6
                        ]
                      }
          tick state

regularAngles :: Int -> Array Number
regularAngles n = map (\x -> toNumber x * (360.0 / (toNumber n))) (0..(n - 1))

tick :: forall eff. State ->  EffWebGL (now :: Now |eff) Unit
tick state = do
--  trace ("tick: " ++ show state.lastTime)
  drawScene state
  state' <- animate state
  return unit
  requestAnimationFrame (tick state')

unpackMilliseconds :: Milliseconds -> Number
unpackMilliseconds (Milliseconds n) = n

animate ::  forall eff. State -> EffWebGL (now :: Now |eff) State
animate state = do
  timeNow <- liftM1 (unpackMilliseconds <<< toEpochMilliseconds) now
  case state.lastTime of
    Nothing -> return state {lastTime = Just timeNow}
    Just lastt ->
      let elapsed = timeNow - lastt in


      return state' {lastTime = Just timeNow,
                       rTri = state.rTri - (30.0 * elapsed) / 1000.0,
                       rSquare = state.rSquare + (75.0 * elapsed) / 1000.0,
                       angles = map (tween elapsed) state'.angles}
  where
    state' =
      if converged state.angles
        then newShape state
        else state

converged :: Array AnimatedNumber -> Boolean
converged xs = all (\(Tuple x y) -> x == y) xs

tail' xs = case tail xs of
                 Just x -> x
                 Nothing -> []

detect :: forall a. (a -> Boolean) -> Array a -> Maybe a
detect f xs = case findIndex f xs of
                   Just i ->  xs !! i
                   Nothing -> Nothing

newShape :: State -> State
newShape state = state {
    angles = nextAngles,
    remainingShapes = rotate state.remainingShapes
  }
  where
    nextAngles :: Array AnimatedNumber
    nextAngles = nub $ animateToNextShape state.remainingShapes state.angles
    rotate xs = case head xs of
                     Nothing -> []
                     Just h -> snoc (tail' xs) h

    animateToNextShape :: Array Shape -> Array AnimatedNumber -> Array AnimatedNumber
    animateToNextShape shapes existing =
      case head shapes of
           Just shape -> animateToNextShape' shape (nub existing)
           Nothing -> []

    animateToNextShape' :: Shape -> Array AnimatedNumber -> Array AnimatedNumber
    animateToNextShape' shape existing =
        sortBy (comparing fst) $ newAngles <> leftovers'
      where
        newAngles :: Array AnimatedNumber
        newAngles = map (\x -> Tuple (nearestPoint (map fst existing) x) x) shape.angles

        leftovers :: Array AnimatedNumber
        leftovers = filter (\(Tuple x _) -> x `notElem` (map fst newAngles)) existing

        leftovers' = map (\(Tuple x _) -> Tuple x (nearestPoint shape.angles x)) leftovers

    nearestPoint :: Array Number -> Number -> Number
    nearestPoint current x =
      let pairs = zip current (tail' current ++ [360.0]) in
      let nearest = detect (\(Tuple l h) -> l <= x && x < h) pairs in
        case nearest of
             Just x -> fst x
             Nothing -> 1.0 -- Fail


tween :: Number -> AnimatedNumber -> AnimatedNumber
tween delta (Tuple current dest) =
  let speed = 0.05 in
  let diff = delta * speed in

  if (abs $ dest - current) < diff
     then Tuple dest dest
     else if current < dest
            then Tuple (current + diff) dest
            else Tuple (current - diff) dest

drawScene :: forall eff. State  -> EffWebGL (now :: Now |eff) Unit
drawScene s = do
      canvasWidth <- getCanvasWidth s.context
      canvasHeight <- getCanvasHeight s.context
      viewport 0 0 canvasWidth canvasHeight
      clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

      let pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
      setUniformFloats s.uPMatrix (M.toArray pMatrix)
      let mvMatrix =
          M.rotate (degToRad s.rTri) (V3.vec3' [0.0, 0.0, 1.0])
            $ M.translate  (V3.vec3 0.0 0.0 (-7.0)) M.identity

      setUniformFloats s.uMVMatrix (M.toArray mvMatrix)

      let angles = map (degToRad <<< fst) (s.angles ++ catMaybes [head s.angles])
      colors <- makeBufferFloat $ concatMap (\_ -> [0.0, 0.5, 1.0, 1.0]) angles
      buf1 <- makeBufferFloat $ concatMap (\x -> [cos x,  sin x,  0.0]) angles

      bindBufAndSetVertexAttr colors s.aVertexColor
      drawArr TRIANGLE_FAN buf1 s.aVertexPosition

      --let mvMatrix' =
      --    M.rotate (degToRad s.rSquare) (V3.vec3' [1.0, 0.0, 0.0])
      --      $ M.translate  (V3.vec3 (1.5) 0.0 (-7.0)) M.identity
      --setUniformFloats s.uMVMatrix (M.toArray mvMatrix')

      --bindBufAndSetVertexAttr s.buf2Colors s.aVertexColor
      --drawArr TRIANGLE_STRIP s.buf2 s.aVertexPosition

-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180.0

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180.0*pi

mkShape :: String -> Array Number -> Shape
mkShape n a = { name : n, angles : a }
