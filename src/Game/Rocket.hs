module Game.Rocket (updateRockets, postUpdateRockets) where
    import Type.Object.Rocket 
    import Type.Physics.GameObject
    import Type.State
    
    updateRockets :: Float -> GameState -> [Rocket]
    updateRockets t gs@GameState{rocketPicture = rp} = rs2
        where 
            rs = (rockets.inGame) gs
            rs2 | null rs = [makeRocket (Pos 0 0) (Vel 0 10) 0 rp]
                | otherwise = rs

    postUpdateRockets :: Float -> GameState -> GameState
    postUpdateRockets t gs = gs