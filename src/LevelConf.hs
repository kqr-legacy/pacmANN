{-# LANGUAGE NoMonomorphismRestriction #-}
module LevelConf where


-- This module is a temporary hack with compile-time constants for the
-- time being!

levelWidth = 19
levelHeight = 22
tileSize = 32

level1 :: [[Char]]
level1 =
    ["###################"
    ,"#        #        #"
    ,"# ##m### # ###m## #"
    ,"#.## ### # ### ##.#"
    ,"#                 #"
    ,"# ## # ##### # ## #"
    ,"#    #m  #   #m   #"
    ,"#### ### # ### ####"
    ,"   # #   m   # #   "
    ,"#### # ## ## # ####"
    ,"#m     #mmm#     m#"
    ,"#### # ##### # ####"
    ,"   # #       # #   "
    ,"#### # ##### # ####"
    ,"#        #        #"
    ,"# ## ### # ### ## #"
    ,"#. #     P     # .#"
    ,"## # # ##### # # ##"
    ,"#    #   #   #    #"
    ,"# ###### # ###### #"
    ,"#                 #"
    ,"###################"
    ]


