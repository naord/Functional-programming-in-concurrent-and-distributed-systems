-author("nir").

% Global Records
-record(flower,{id, type, status, pointsLifeTime}).
-record(gardener,{id, type, state, location = {0,0}}).

% Constant
-define(screen_width , 1360).
-define(screen_height, 960).

% Global Variables
% Gardener variables

-define(handle, 10).
-define(squareSize,80).
-define(walkTime, 10).
-define(gardenSize, 1000).

% Images paths
-define(imagesPath,  "/home/nirkov/Desktop/TheNightmareGarden/TheNightmareGarden/pics").
-define(irisPath    , ?imagesPath ++ "/iris").
-define(redPath     , ?imagesPath ++ "/red").
-define(lawnPath    , ?imagesPath ++ "/Lawn").
-define(gardenerPath, ?imagesPath ++ "/gardener").

% Flower images paths

% iris -
-define(iris_r, ?irisPath ++ "/irisRight.png").
-define(iris_l, ?irisPath ++ "/irisLeft.png").

-define(iris_r_wilted, ?irisPath ++ "/wilted_irisRight.png").
-define(iris_l_wilted,  ?irisPath ++ "/wilted_irisLeft.png").

-define(iris_r_pests_ant, ?irisPath ++ "/pests_irisRight_ant.png").
-define(iris_l_pests_ant, ?irisPath ++ "/pests_irisLeft_ant.png").

-define(iris_r_pests_purple, ?irisPath ++ "/pests_irisRight_purple.png").
-define(iris_l_pests_purple, ?irisPath ++ "/pests_irisLeft_purple.png").

-define(iris_r_pests_green, ?irisPath ++ "/pests_irisRight_green.png").
-define(iris_l_pests_green, ?irisPath ++ "/pests_irisLeft_green.png").

% red -
-define(red_r, ?redPath ++ "/redRight.png").
-define(red_l, ?redPath ++ "/redLeft.png").

-define(red_r_wilted, ?redPath ++ "/wilted_redRight.png").
-define(red_l_wilted,  ?redPath ++ "/wilted_redLeft.png").

-define(red_r_pests_ant, ?redPath ++ "/pests_redRight_ant.png").
-define(red_l_pests_ant, ?redPath ++ "/pests_redLeft_ant.png").

-define(red_r_pests_purple, ?redPath ++ "/pests_redRight_purple.png").
-define(red_l_pests_purple, ?redPath ++ "/pests_redLeft_purple.png").

-define(red_r_pests_green, ?redPath ++ "/pests_redRight_green.png").
-define(red_l_pests_green, ?redPath ++ "/pests_redLeft_green.png").


% Background images path
-define(lawn_background  , ?lawnPath ++ "/Background_1360_960.png").
-define(lawn_piece       , ?lawnPath ++ "/PieceOfLawn.png").

% Gardener images path
-define(nir_left_first   , ?gardenerPath ++ "/nirLeftFirst.png").
-define(nir_left_second  , ?gardenerPath ++ "/nirLeftSec.png").
-define(nir_right_first  , ?gardenerPath ++ "/nirRightFirst.png").
-define(nir_right_second , ?gardenerPath ++ "/nirRightSec.png").
-define(nir_sit          , ?gardenerPath ++ "/nir_sit.png").


















