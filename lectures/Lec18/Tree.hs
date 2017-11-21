
-- | Tree Fractal.
--      Based on ANUPlot code by Clem Baker-Finch.
--
-- https://github.com/benl23x5/gloss/blob/master/gloss-examples/
-- Copyright (c) 2010-2016 The Gloss Development Team
--
import Graphics.Gloss

main :: IO ()
main =  animate (InWindow "Tree" (500, 650) (20,  20))
                black (picture 5) -- we changed this from 4 to 5 iterations


-- The picture is a tree fractal, graded from brown to green
picture :: Int -> Float -> Picture
picture degree time
        = Translate 0 (-300)
        $ tree degree time (dim $ dim brown)


-- Basic stump shape
stump :: Color -> Picture
stump color
        = Color color
        $ Polygon [(30,0), (15,300), (-15,300), (-30,0)]


-- Make a tree fractal.
tree    :: Int          -- Fractal degree
        -> Float        -- time
        -> Color        -- Color for the stump
        -> Picture

tree 0 time color = stump color
tree n time color
 = let  smallTree
                = Rotate (tan time * sin time)
                $ Scale 0.5 0.5
                $ tree (n-1) (- time) (greener color)
   in   Pictures
                [ stump color
                , Translate 0 300 $ smallTree
                , Translate 0 240 $ Rotate 20    smallTree
                , Translate 0 180 $ Rotate (-20) smallTree
                , Translate 0 120 $ Rotate 40    smallTree
                , Translate 0  60 $ Rotate (-40) smallTree
                -- , Translate 0  60 $ Rotate 40 smallTree  -- we added this branch
                ]


-- A starting colour for the stump
brown :: Color
brown =  makeColorI 139 100 35  255


-- Make this color a little greener
greener :: Color -> Color
greener c = mixColors 1 10 red c

