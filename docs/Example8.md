## Module Example8

#### `MyBindings`

``` purescript
type MyBindings = (aVertexPosition :: Attribute Vec3, aVertexNormal :: Attribute Vec3, aTextureCoord :: Attribute Vec2, uPMatrix :: Uniform Mat4, uMVMatrix :: Uniform Mat4, uNMatrix :: Uniform Mat4, uSampler :: Uniform Sampler2D, uUseLighting :: Uniform Bool, uAmbientColor :: Uniform Vec3, uLightingDirection :: Uniform Vec3, uDirectionalColor :: Uniform Vec3, uAlpha :: Uniform Float)
```

#### `shaders`

``` purescript
shaders :: Shaders {  | MyBindings }
```

#### `cubeV`

``` purescript
cubeV :: Array Number
```

#### `vertexNormals`

``` purescript
vertexNormals :: Array Number
```

#### `texCoo`

``` purescript
texCoo :: Array Number
```

#### `cvi`

``` purescript
cvi :: Array Int
```

#### `State`

``` purescript
type State bindings = { context :: WebGLContext, bindings :: { webGLProgram :: WebGLProg | bindings }, cubeVertices :: Buffer Float32, cubeVerticesNormal :: Buffer Float32, textureCoords :: Buffer Float32, cubeVertexIndices :: Buffer Uint16, texture :: WebGLTex, lastTime :: Maybe Number, xRot :: Number, xSpeed :: Number, yRot :: Number, ySpeed :: Number, z :: Number, currentlyPressedKeys :: Array Int }
```

#### `main`

``` purescript
main :: Eff (console :: CONSOLE, alert :: Alert, now :: Now) Unit
```

#### `tick`

``` purescript
tick :: forall h eff. STRef h (State MyBindings) -> EffWebGL (st :: ST h, console :: CONSOLE, now :: Now | eff) Unit
```

#### `unpackMilliseconds`

``` purescript
unpackMilliseconds :: Milliseconds -> Number
```

#### `animate`

``` purescript
animate :: forall h eff. STRef h (State MyBindings) -> EffWebGL (st :: ST h, now :: Now | eff) Unit
```

#### `drawScene`

``` purescript
drawScene :: forall h eff. STRef h (State MyBindings) -> EffWebGL (st :: ST h | eff) Unit
```

#### `setBlending`

``` purescript
setBlending :: forall eff. State MyBindings -> EffWebGL eff Unit
```

#### `setLightning`

``` purescript
setLightning :: forall eff. State MyBindings -> EffWebGL eff Unit
```

#### `radToDeg`

``` purescript
radToDeg :: Number -> Number
```

Convert from radians to degrees.

#### `degToRad`

``` purescript
degToRad :: Number -> Number
```

Convert from degrees to radians.

#### `handleKeys`

``` purescript
handleKeys :: forall h eff. STRef h (State MyBindings) -> EffWebGL (console :: CONSOLE, st :: ST h | eff) Unit
```

#### `handleKeyD`

``` purescript
handleKeyD :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
```

#### `handleKeyU`

``` purescript
handleKeyU :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
```


