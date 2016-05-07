module Camera
    (rotationMatrix,
    translationMatrix,
    modelMatrix,
    viewMatrix,
    projectionMatrix,
    mvMatrix,
    mvpMatrix,
    normalize) where

import Graphics.GPipe

rotationMatrix :: VFloat -> VFloat -> VFloat -> V4 (V4 VFloat)
rotationMatrix x y z = V4 row1 row2 row3 row4
    where
        row1 = V4 ((cos y) * (cos z))   (((cos z) * (sin x) * (sin y)) - ((cos x) * (sin z)))   (((cos x) * (cos z) * (sin y)) + ((sin x) * (sin z)))   0
        row2 = V4 ((cos y) * (sin z))   (((cos x) * (cos z)) + ((sin x) * (sin y) * (sin z)))   (((cos x) * (sin y) * (sin z)) - ((cos z) * (sin x)))   0
        row3 = V4 ((-1.0) * (sin y))    ((cos y) * (sin x))                                     ((cos x) * (cos y))                                     0
        row4 = V4 0                     0                                                       0                                                       1

translationMatrix :: VFloat -> VFloat -> VFloat -> V4 (V4 VFloat)
translationMatrix x y z = V4 row1 row2 row3 row4
    where
        row1 = V4 1 0 0 x
        row2 = V4 0 1 0 y
        row3 = V4 0 0 1 z
        row4 = V4 0 0 0 1

modelMatrix :: VFloat -> VFloat -> VFloat -> VFloat -> VFloat -> VFloat -> V4 (V4 VFloat)
modelMatrix rX rY rZ tX tY tZ = (translationMatrix tX tY tZ) !*! (rotationMatrix rX rY rZ)

viewMatrix :: V4 (V4 VFloat)
viewMatrix = translationMatrix 0.0 0.0 (-3.0)

mvMatrix :: VFloat -> VFloat -> VFloat -> VFloat -> VFloat -> VFloat -> V4 (V4 VFloat)
mvMatrix rX rY rZ tX tY tZ = viewMatrix !*! (modelMatrix rX rY rZ tX tY tZ)

projectionMatrix :: V4 (V4 VFloat)
projectionMatrix = perspective (pi / 4.0) 1.0 0.1 10.0

mvpMatrix :: VFloat -> VFloat -> VFloat -> VFloat -> VFloat -> VFloat -> V4 (V4 VFloat)
mvpMatrix rX rY rZ tX tY tZ = projectionMatrix !*! viewMatrix !*! modelMatrix rX rY rZ tX tY tZ
