module Game.Rocket (updateRockets, postUpdateRockets) where
    import Data.Maybe
    import Type.Object.Rocket 
    import Type.Physics.GameObject
    import Type.State
    import Type.Object.Player as Player
    import Type.IO.Input
    import Game.Object
    import Physics.Collisions
    
    updateRockets :: Float -> GameState -> [Rocket]
    updateRockets t gs@GameState{inputState = ks, rocketPicture = rp, inGame = igs}
        | keyDown ks Shoot && (cooldown.player) igs == 0 = makeRocket newPos newVel r rp : rs
        | otherwise = rs
        where 
            ply = (Player.obj . player) igs
            p = pos ply
            r = rot ply + pi / 2
            newPos = Pos (posX p + sin r * radius ply) (posY p + cos r * radius ply)
            newVel = Vel (sin r * 300) (cos r * 300) + vel ply
            rs = mapMaybe (`rocketHitAsteroids` asteroids igs) (rockets igs)

    postUpdateRockets :: Float -> GameState -> GameState
    postUpdateRockets t gs@GameState{inGame = igs@InGameState{rockets = rs}} = 
        gs{inGame = igs{rockets = mapMaybe (`removeOutOfBounds` gs) rs}}